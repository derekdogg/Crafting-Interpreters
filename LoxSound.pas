unit LoxSound;

interface

uses
  Chunk_Types;

procedure RegisterSoundNatives;
procedure FreeSound;
procedure StopAllSound;

implementation

uses
  SysUtils, Math, MMSystem;

const
  NUM_CHANNELS = 4;
  SAMPLE_RATE  = 44100;
  SEQ_LOOP_REPEATS = 30;  // repeat melody this many times for "looping"
  MAX_SEQ_BYTES = 30 * 1024 * 1024;  // 30 MB max sequence buffer (~5 min mono 16-bit)
  MAX_NOTE_DURATION_MS = 30000;  // 30 seconds per note max
  MAX_NOTE_FREQ = 20000;  // 20 kHz max frequency

var
  FWaveOut: array[0..NUM_CHANNELS-1] of HWAVEOUT;
  FWaveHdr: array[0..NUM_CHANNELS-1] of TWaveHdr;
  FBuf: array[0..NUM_CHANNELS-1] of TArray<Byte>;
  FNextCh: Integer;
  FReady: Boolean;

  // Dedicated music sequencer channel
  FSeqWaveOut: HWAVEOUT;
  FSeqWaveHdr: TWaveHdr;
  FSeqBuf: TArray<Byte>;
  FSeqReady: Boolean;
  FSeqPlaying: Boolean;

procedure InitAudio;
var
  fmt: TWaveFormatEx;
  i: Integer;
  anyOpen: Boolean;
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
  anyOpen := False;
  for i := 0 to NUM_CHANNELS - 1 do
  begin
    FWaveOut[i] := 0;
    FillChar(FWaveHdr[i], SizeOf(TWaveHdr), 0);
    if waveOutOpen(@FWaveOut[i], WAVE_MAPPER, @fmt, 0, 0, CALLBACK_NULL) = MMSYSERR_NOERROR then
      anyOpen := True
    else
      FWaveOut[i] := 0;
  end;
  // Open dedicated sequencer channel
  FSeqWaveOut := 0;
  FillChar(FSeqWaveHdr, SizeOf(TWaveHdr), 0);
  if waveOutOpen(@FSeqWaveOut, WAVE_MAPPER, @fmt, 0, 0, CALLBACK_NULL) = MMSYSERR_NOERROR then
    anyOpen := True
  else
    FSeqWaveOut := 0;
  FSeqReady := (FSeqWaveOut <> 0);
  FSeqPlaying := False;
  FNextCh := 0;
  FReady := anyOpen;
end;

procedure StopAllSound;
var
  i: Integer;
begin
  // Halt any in-flight playback on every channel + the sequencer, but
  // leave the wave devices open so the next script run can keep
  // playing without paying the open/format-negotiation cost. Called when
  // a script ends (normally or via abort) so audio doesn't outlive it.
  if FReady then
  begin
    for i := 0 to NUM_CHANNELS - 1 do
      if FWaveOut[i] <> 0 then
        waveOutReset(FWaveOut[i]);
    if FSeqWaveOut <> 0 then
      waveOutReset(FSeqWaveOut);
    FSeqPlaying := False;
  end;
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
  // Close sequencer channel
  if FSeqWaveOut <> 0 then
  begin
    waveOutReset(FSeqWaveOut);
    if FSeqWaveHdr.dwFlags and WHDR_PREPARED <> 0 then
      waveOutUnprepareHeader(FSeqWaveOut, @FSeqWaveHdr, SizeOf(TWaveHdr));
    waveOutClose(FSeqWaveOut);
    FSeqWaveOut := 0;
  end;
  FSeqPlaying := False;
  FSeqReady := False;
  FReady := False;
end;

procedure PlayRaw(const samples: TArray<Byte>);
var
  ch, startCh, i: Integer;
begin
  if Length(samples) = 0 then Exit;
  if not FReady then InitAudio;
  if not FReady then Exit; // no audio device available
  // Find next valid channel, skipping dead ones
  startCh := FNextCh;
  ch := -1;
  for i := 0 to NUM_CHANNELS - 1 do
  begin
    if FWaveOut[(startCh + i) mod NUM_CHANNELS] <> 0 then
    begin
      ch := (startCh + i) mod NUM_CHANNELS;
      FNextCh := (ch + 1) mod NUM_CHANNELS;
      Break;
    end;
  end;
  if ch < 0 then Exit; // all channels dead
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
  env, vol: Double;
  buf: TArray<Byte>;
begin
  if (argCount < 2) or (argCount > 3) then
  begin
    RuntimeError('playTone() takes 2-3 arguments (frequency, durationMs [, volume]).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) then
  begin
    RuntimeError('playTone() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  vol := 1.0;
  if (argCount = 3) then
  begin
    if args[2].ValueKind <> vkNumber then
    begin
      RuntimeError('playTone() volume must be a number (0.0 to 1.0).');
      Exit(CreateNilValue);
    end;
    vol := args[2].NumberValue;
    if vol < 0 then vol := 0;
    if vol > 1 then vol := 1;
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
    if ((Int64(i) * freq * 2) div SAMPLE_RATE) mod 2 = 0 then
      sv := 10000
    else
      sv := -10000;
    // Sustain then fade last 10%
    env := 1.0 - (i / n);
    if env > 0.1 then env := 1.0
    else env := env / 0.1;
    PutSample(buf, i, Trunc(sv * env * vol));
  end;
  PlayRaw(buf);
  Result := CreateNilValue;
end;

function playSweepNative(argCount: integer; args: pValue): TValue;
var
  startFreq, endFreq, durMs, n, i: Integer;
  freq, phase, phaseInc, env, vol: Double;
  sv: SmallInt;
  buf: TArray<Byte>;
begin
  if (argCount < 3) or (argCount > 4) then
  begin
    RuntimeError('playSweep() takes 3-4 arguments (startFreq, endFreq, durationMs [, volume]).');
    Exit(CreateNilValue);
  end;
  if (args[0].ValueKind <> vkNumber) or (args[1].ValueKind <> vkNumber) or
     (args[2].ValueKind <> vkNumber) then
  begin
    RuntimeError('playSweep() arguments must be numbers.');
    Exit(CreateNilValue);
  end;
  vol := 1.0;
  if (argCount = 4) then
  begin
    if args[3].ValueKind <> vkNumber then
    begin
      RuntimeError('playSweep() volume must be a number (0.0 to 1.0).');
      Exit(CreateNilValue);
    end;
    vol := args[3].NumberValue;
    if vol < 0 then vol := 0;
    if vol > 1 then vol := 1;
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
    PutSample(buf, i, Trunc(sv * env * vol));
  end;
  PlayRaw(buf);
  Result := CreateNilValue;
end;

function playNoiseNative(argCount: integer; args: pValue): TValue;
var
  durMs, n, i: Integer;
  sv: SmallInt;
  env, vol: Double;
  buf: TArray<Byte>;
begin
  if (argCount < 1) or (argCount > 2) then
  begin
    RuntimeError('playNoise() takes 1-2 arguments (durationMs [, volume]).');
    Exit(CreateNilValue);
  end;
  if args[0].ValueKind <> vkNumber then
  begin
    RuntimeError('playNoise() argument must be a number.');
    Exit(CreateNilValue);
  end;
  vol := 1.0;
  if (argCount = 2) then
  begin
    if args[1].ValueKind <> vkNumber then
    begin
      RuntimeError('playNoise() volume must be a number (0.0 to 1.0).');
      Exit(CreateNilValue);
    end;
    vol := args[1].NumberValue;
    if vol < 0 then vol := 0;
    if vol > 1 then vol := 1;
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
    PutSample(buf, i, Trunc(sv * env * vol));
  end;
  PlayRaw(buf);
  Result := CreateNilValue;
end;

function playSequenceNative(argCount: integer; args: pValue): TValue;
var
  notesArr, noteArr: pObjArray;
  noteCount, i, j, freq, durMs, n, sampleIdx, repeats: Integer;
  totalSamples: Int64;
  bufferBytes: Int64;
  notePos, waveType: Integer;
  doLoop: Boolean;
  phase, phaseInc, env, vibrato, vibratoPhase: Double;
  sustainLevel, sample: Double;
  attackSamples, decaySamples, releaseSamples: Integer;
  sv: SmallInt;
begin
  if argCount <> 2 then
  begin
    RuntimeError('playSequence() takes 2 arguments (notesArray, loop).');
    Exit(CreateNilValue);
  end;
  if not isArray(args[0]) then
  begin
    RuntimeError('playSequence() first argument must be an array of [freq, durationMs, waveType?] notes.');
    Exit(CreateNilValue);
  end;
  if args[1].ValueKind <> vkBoolean then
  begin
    RuntimeError('playSequence() second argument must be a boolean (loop).');
    Exit(CreateNilValue);
  end;
  doLoop := args[1].BooleanValue;

  if not FReady then InitAudio;
  if not FSeqReady then
  begin
    Result := CreateNilValue;
    Exit;
  end;

  // Stop any currently playing sequence
  if FSeqPlaying then
  begin
    waveOutReset(FSeqWaveOut);
    if FSeqWaveHdr.dwFlags and WHDR_PREPARED <> 0 then
      waveOutUnprepareHeader(FSeqWaveOut, @FSeqWaveHdr, SizeOf(TWaveHdr));
    FSeqPlaying := False;
  end;

  notesArr := pObjArray(args[0].ObjValue);
  noteCount := notesArr^.Count;
  if noteCount = 0 then
  begin
    Result := CreateNilValue;
    Exit;
  end;

  // Calculate total samples for one pass
  totalSamples := 0;
  for i := 0 to noteCount - 1 do
  begin
    if not isArray(notesArr^.Elements[i]) then
    begin
      RuntimeError('playSequence() each note must be [freq, durationMs, waveType?] (waveType: 0=pulse, 1=triangle, 2=saw, 3=noise).');
      Exit(CreateNilValue);
    end;
    noteArr := pObjArray(notesArr^.Elements[i].ObjValue);
    if noteArr^.Count < 2 then
    begin
      RuntimeError('playSequence() each note must have at least [freq, durationMs].');
      Exit(CreateNilValue);
    end;
    if (noteArr^.Elements[0].ValueKind <> vkNumber) or (noteArr^.Elements[1].ValueKind <> vkNumber) then
    begin
      RuntimeError('playSequence() note freq and duration must be numbers.');
      Exit(CreateNilValue);
    end;
    durMs := Trunc(noteArr^.Elements[1].NumberValue);
    if durMs < 1 then durMs := 1;
    if durMs > MAX_NOTE_DURATION_MS then durMs := MAX_NOTE_DURATION_MS;
    totalSamples := totalSamples + Int64(SAMPLE_RATE) * durMs div 1000;
  end;

  // Determine repetitions
  if doLoop then
    repeats := SEQ_LOOP_REPEATS
  else
    repeats := 1;

  // Check buffer size before allocating
  bufferBytes := totalSamples * repeats * 2;
  if bufferBytes > MAX_SEQ_BYTES then
  begin
    RuntimeError('playSequence() sequence too long (exceeds max buffer size).');
    Exit(CreateNilValue);
  end;

  // Allocate buffer for all repeats
  SetLength(FSeqBuf, Integer(bufferBytes));

  // Render with rich synthesis
  sampleIdx := 0;
  for j := 0 to repeats - 1 do
  begin
    for i := 0 to noteCount - 1 do
    begin
      noteArr := pObjArray(notesArr^.Elements[i].ObjValue);
      freq := Trunc(noteArr^.Elements[0].NumberValue);
      if freq > MAX_NOTE_FREQ then freq := MAX_NOTE_FREQ;
      durMs := Trunc(noteArr^.Elements[1].NumberValue);
      if durMs < 1 then durMs := 1;
      if durMs > MAX_NOTE_DURATION_MS then durMs := MAX_NOTE_DURATION_MS;
      n := (SAMPLE_RATE * durMs) div 1000;
      if n < 1 then n := 1;

      // Optional waveform type: [freq, ms, type]
      // 0 = pulse25 (default), 1 = triangle, 2 = sawtooth, 3 = noise
      waveType := 0;
      if (noteArr^.Count >= 3) and (noteArr^.Elements[2].ValueKind = vkNumber) then
      begin
        waveType := Trunc(noteArr^.Elements[2].NumberValue);
        if (waveType < 0) or (waveType > 3) then
          waveType := 0;
      end;

      if freq <= 0 then
      begin
        // Rest note (silence)
        FillChar(FSeqBuf[sampleIdx * 2], n * 2, 0);
        sampleIdx := sampleIdx + n;
      end
      else
      begin
        // ADSR envelope parameters (in samples)
        attackSamples := (SAMPLE_RATE * 8) div 1000;   // 8ms attack
        decaySamples := (SAMPLE_RATE * 40) div 1000;   // 40ms decay
        releaseSamples := (SAMPLE_RATE * 15) div 1000; // 15ms release
        sustainLevel := 0.65;

        phase := 0;
        vibratoPhase := 0;
        for notePos := 0 to n - 1 do
        begin
          // Vibrato: 5Hz wobble, ±3Hz depth, kicks in after 60ms
          vibrato := 0;
          if notePos > (SAMPLE_RATE * 60) div 1000 then
          begin
            vibratoPhase := vibratoPhase + (5.0 / SAMPLE_RATE);
            vibrato := Sin(vibratoPhase * 2 * Pi) * 3.0;
          end;

          phaseInc := (freq + vibrato) / SAMPLE_RATE;
          phase := phase + phaseInc;
          phase := phase - Floor(phase);

          // Waveform generation
          case waveType of
            1: // Triangle wave (smooth, mellow)
              sample := (2.0 * Abs(2.0 * phase - 1.0) - 1.0) * 2000;
            2: // Sawtooth (buzzy, harmonically rich)
              sample := (2.0 * phase - 1.0) * 1500;
            3: // Noise (percussion)
              sample := (Random - 0.5) * 3000;
          else
            // Pulse 25% duty cycle (bright, chiptune character)
            if phase < 0.25 then sample := 1600 else sample := -1600;
          end;

          // ADSR envelope
          if notePos < attackSamples then
            // Attack
            env := notePos / attackSamples
          else if notePos < attackSamples + decaySamples then
            // Decay: from 1.0 down to sustainLevel
            env := 1.0 - (1.0 - sustainLevel) * ((notePos - attackSamples) / decaySamples)
          else if notePos >= n - releaseSamples then
            // Release
            env := sustainLevel * ((n - notePos) / releaseSamples)
          else
            // Sustain
            env := sustainLevel;

          if env < 0 then env := 0;
          if env > 1 then env := 1;

          sv := Trunc(sample * env);
          PutSample(FSeqBuf, sampleIdx, sv);
          sampleIdx := sampleIdx + 1;
        end;
      end;
    end;
  end;

  // Play
  FillChar(FSeqWaveHdr, SizeOf(TWaveHdr), 0);
  FSeqWaveHdr.lpData := PAnsiChar(@FSeqBuf[0]);
  FSeqWaveHdr.dwBufferLength := Length(FSeqBuf);
  if waveOutPrepareHeader(FSeqWaveOut, @FSeqWaveHdr, SizeOf(TWaveHdr)) = MMSYSERR_NOERROR then
  begin
    waveOutWrite(FSeqWaveOut, @FSeqWaveHdr, SizeOf(TWaveHdr));
    FSeqPlaying := True;
  end;
  Result := CreateNilValue;
end;

function stopSequenceNative(argCount: integer; args: pValue): TValue;
begin
  if FSeqPlaying and (FSeqWaveOut <> 0) then
  begin
    waveOutReset(FSeqWaveOut);
    if FSeqWaveHdr.dwFlags and WHDR_PREPARED <> 0 then
      waveOutUnprepareHeader(FSeqWaveOut, @FSeqWaveHdr, SizeOf(TWaveHdr));
    FSeqPlaying := False;
  end;
  Result := CreateNilValue;
end;

procedure RegisterSoundNatives;
begin
  defineNative('playTone', playToneNative, -1);
  defineNative('playSweep', playSweepNative, -1);
  defineNative('playNoise', playNoiseNative, -1);
  defineNative('playSequence', playSequenceNative, 2);
  defineNative('stopSequence', stopSequenceNative, 0);
end;

initialization
  FReady := False;

finalization
  FreeSound;

end.
