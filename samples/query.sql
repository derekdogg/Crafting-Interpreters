SELECT TOP 10
  Created_By,
  COUNT(*) AS RequestCount,
  SUM(HrUnits) AS TotalUnits
FROM dbo.Leave_Requests_Workflow
GROUP BY Created_By
ORDER BY RequestCount DESC
