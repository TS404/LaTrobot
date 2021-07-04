# Collects ORCIDs from symprptprd

SELECT DISTINCT
    U.[ID] AS 'User ID',
    U.[Computed Name Alphabetical],
    UIS.[Name],
    CASE WHEN UI.[Identifier Scheme ID] = 9 THEN UI.[Identifier Value] END AS 'ORCID',
    ROW_NUMBER() OVER(PARTITION BY U.[ID] ORDER BY UI.[Identifier Value] DESC) rn

FROM [symprptprd-reporting].[dbo].[User] U
    LEFT JOIN [symprptprd-reporting].[dbo].[User Identifier] UI ON UI.[User ID] = U.[ID]
    LEFT JOIN [symprptprd-reporting].[dbo].[Identifier Scheme] UIS ON UI.[Identifier Scheme ID] = UIS.ID
    
WHERE UI.[Is Claimed] = 1 AND UI.[Identifier Scheme ID] = 9