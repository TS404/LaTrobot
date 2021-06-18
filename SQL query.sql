  
SELECT DISTINCT
    UD.*,
    U.[Arrive Date],
    U.[Leave Date],
    U.[Subgroup],
    U.[Classification],
    U.[Position],
    CASE WHEN U.[photo hash] IS NOT NULL THEN 'Has photo'END AS 'Photo Note',
    U.[overview],
    UE.[Expertise],
    U.[research-interests] AS 'Research Interests',
    U.[Is Current Staff]
        
FROM [symprptprd-reporting].[dbo].[User] U
    LEFT JOIN [dbo].[UserData] UD ON U.[ID] = UD.[User ID]
    LEFT JOIN [symprptprd-reporting].[dbo].[Professional Activity User Relationship] PAUR ON U.[ID] = PAUR.[User ID]
    LEFT JOIN [symprptprd-reporting].[dbo].[Professional Activity] UPA ON PAUR.[Professional Activity ID] = UPA.[ID]
    LEFT JOIN [symprptprd-reporting].[dbo].[User Label] UL ON U.[ID] = UL.[User ID]
    LEFT JOIN [dbo].[Users_Expertise] UE ON U.[ID] = UE.[User ID]

ORDER BY UD.[Computed Name Alphabetical]



