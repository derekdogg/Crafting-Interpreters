unit LoxSound;

interface

uses
  Chunk_Types;

procedure RegisterSoundNatives;
procedure FreeSound;

implementation

uses
  SysUtils, MMSystem;

const
  NUM_CHANNELS = 4;
  SAMPLE_RATE  = 44100;

var
  FWaveOut: array[0..NUM_CHANNELS-1] of HWAVEOUT;
  FWaveHdr: array[0..NUM_CHANNELS-1] of TWaveHdr;
  FBuf: array[0..NUM_CHANNELS-1] of TArray<Byte>;
  FNextCh: Integer;
  FReady: Boolean;

procedure InitAudio;
var
  fmt: TWaveFormatEx;
  i: Integer;
begin
  if FReady then Exit;
  FillChar(fmt, SizeOf(fmt), 0);
  fmt.wFormatTag := WAVE_FORMAT_PCM;
  fmt.nChannels := 1;
  fmt.nSamplesPerSec := SAMPLE_RATE;
  fmt.wBitsPerSample := 16;
  fmt.nBlockAlign := 2;
  fmt.nAvgBytesPerSec := SAMPLE_RATE * 2;
  fmt.cbSize := 0;
  for i := 0 to NUM_CHANNELS - 1 do
  begin
    FWaveOut[i] := 0;
    FillChar(FWaveHdr[i], SizeOf(TWaveHdr), 0);
    if waveOutOpen(@FWaveOut[i], WAVE_MAPPER, @fmt, 0, 0, CALLBACK_NULL) <> MMSYSERR_NOERROR then
      FWaveOut[i] := 0;
  end;
  FNextCh := 0;
  FReady := True;
  Randomize;
end;

procedure FreeSound;
var
  i: Integer;
begin
  if not FReady then Exit;
  for i := 0 to NUM_CHANNELS - 1 do
    if FWaveOut[i] <> 0 then
    begin
      waveOutReset(FWaveOut[i]);
      if FWaveHdr[i].dwFlags and WHDR_PREPARED <> 0 then
        waveOutUnprepareHeader(FWaveOut[i], @FWaveHdr[i], SizeOf(TWaveHdr));
      waveOutClose(FWaveOut[i]);
      FWaveOut[i] := 0;
    end;
  FReady := False;
end;

procedure PlayRaw(const samples: TArray<Byte>);
var
  ch: Integer;
begin
  if Length(samples) = 0 then Exit;
  if not FReady then InitAudio;
  ch := FNextCh;
  FNextCh := (FNextCh + 1) mod NUM_CHANNELS;
  if FWaveOut[ch] = 0 then Exit;
  // Stop current sound on this channel
  waveOutReset(FWaveOut[ch]);
  if FWaveHdr[ch].dwFlags and WHDR_PREPARED <> 0 then
    waveOutUnprepareHeader(FWaveOut[ch], @FWaveHdr[ch], SizeOf(TWaveHdr));
  // Keep buffer alive for async playback
  FBuf[ch] := samples;
  FillChar(FWaveHdr[ch], SizeOf(TWaveHdr), 0);
  FWaveHdr[ch].lpData := PAnsiChar(@FBuf[ch][0]);
  FWaveHdr[ch].dwBufferLength := Length(samples);
  if waveOutPrepareHeader(FWaveOut[ch], @FWaveHdr[ch], SizeOf(TWaveHdr)) = MMSYSERR_NOERROR then
    waveOutWrite(FWaveOut[ch], @FWaveHdr[ch], SizeOf(TWaveHdr));
end;

procedure PutSample(var buf: TArray<Byte>; index: Integer; value: SmallInt);
begin
  buf[index * 2]     := Byte(Word(value) and $FF);
  buf[index * 2 + 1] := Byte(Word(value) shr 8);
end;

function playToneNative(argCount: integer; args: pValue): TValue;
var
  freq, durMs, n, i: Integer;
  sv: SmallInt;
  env: Double;
  buf: TArray<Byte>;
begin
  if argCount <> 2 then
  begin
    RuntimeError('playTone() takes 2 arguments (frequency, durationMs).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) then
  begin
    RuntimeError('playTone() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  freq := Trunc(args[0].NumberValue);
  durMs := Trunc(args[1].NumberValue);
  if (freq < 20) or (freq > 20000) then
  begin
    RuntimeError('playTone() frequency must be between 20 and 20000.');
    Exit(CreateNilValue);
  end;
  if (durMs < 1) or (durMs > 5000) then
  begin
    RuntimeError('playTone() duration must be between 1 and 5000 ms.');
    Exit(CreateNilValue);
  end;

  n := (SAMPLE_RATE * durMs) div 1000;
  SetLength(buf, n * 2);
  for i := 0 to n - 1 do
  begin
    if ((i * freq * 2) div SAMPLE_RATE) mod 2 = 0 then
      sv := 10000
    else
      sv := -10000;
    // Sustain then fade last 10%
    env := 1.0 - (i / n);
    if env > 0.1 then env := 1.0
    else env := env / 0.1;
    PutSample(buf, i, Trunc(sv * env));
  end;
  PlayRaw(buf);
  Result := CreateNilValue;
end;

function playSweepNative(argCount: integer; args: pValue): TValue;
var
  startFreq, endFreq, durMs, n, i: Integer;
  freq, phase, phaseInc, env: Double;
  sv: SmallInt;
  buf: TArray<Byte>;
begin
  if argCount <> 3 then
  begin
    RuntimeError('playSweep() takes 3 arguments (startFreq, endFreq, durationMs).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) then
  begin
    RuntimeError('playSweep() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  startFreq := Trunc(args[0].NumberValue);
  endFreq := Trunc(args[1].NumberValue);
  durMs := Trunc(args[2].NumberValue);
  if (startFreq < 20) or (startFreq > 20000) or
     (endFreq < 20) or (endFreq > 20000) then
  begin
    RuntimeError('playSweep() frequencies must be between 20 and 20000.');
    Exit(CreateNilValue);
  end;
  if (durMs < 1) or (durMs > 5000) then
  begin
    RuntimeError('playSweep() duration must be between 1 and 5000 ms.');
    Exit(CreateNilValue);
  end;

  n := (SAMPLE_RATE * durMs) div 1000;
  SetLength(buf, n * 2);
  phase := 0;
  for i := 0 to n - 1 do
  begin
    freq := startFreq + (endFreq - startFreq) * (i / n);
    phaseInc := freq / SAMPLE_RATE;
    phase := phase + phaseInc;
    phase := phase - Trunc(phase);
    if phase < 0.5 then sv := 10000 else sv := -10000;
    env := 1.0 - (i / n);
    PutSample(buf, i, Trunc(sv * env));
  end;
  PlayRaw(buf);
  Result := CreateNilValue;
end;

function playNoiseNative(argCount: integer; args: pValue): TValue;
var
  durMs, n, i: Integer;
  sv: SmallInt;
  env: Double;
  buf: TArray<Byte>;
begin
  if argCount <> 1 then
  begin
    RuntimeError('playNoise() takes 1 argument (durationMs).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkNumber then
  begin
    RuntimeError('playNoise() argument must be a number.');
    Exit(CreateNilValue);
  end;
  durMs := Trunc(args[0].NumberValue);
  if (durMs < 1) or (durMs > 5000) then
  begin
    RuntimeError('playNoise() duration must be between 1 and 5000 ms.');
    Exit(CreateNilValue);
  end;

  n := (SAMPLE_RATE * durMs) div 1000;
  SetLength(buf, n * 2);
  for i := 0 to n - 1 do
  begin
    sv := SmallInt(Random(20000) - 10000);
    env := 1.0 - (i / n);
    env := env * env;
    PutSample(buf, i, Trunc(sv * env));
  end;
  PlayRaw(buf);
  Result := CreateNilValue;
end;

procedure RegisterSoundNatives;
begin
  defineNative('playTone', playToneNative, 2);
  defineNative('playSweep', playSweepNative, 3);
  defineNative('playNoise', playNoiseNative, 1);
end;

initialization
  FReady := False;

finalization
  FreeSound;

end.
