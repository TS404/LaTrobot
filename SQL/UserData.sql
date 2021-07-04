# Collates user data from symprptprd

SELECT DISTINCT
    U.[Computed Name Alphabetical],
    U.[ID] AS 'User ID',
    U.[Proprietary ID] AS 'Staff ID',
    LOWER (U.[Username]) AS 'Username',
    U.[Email],
    UIO.[ORCID],
    UIS.[ScopusID],
    UIR.[ResearcherID],
    U.[Department],
    CASE WHEN LUDSC.[School] IS NULL THEN LDSC.[School] ELSE LUDSC.[School] END AS 'School',
    CASE WHEN LUDSC.[College] IS NULL THEN LDSC.[College] ELSE LUDSC.[College] END AS 'College'

 

FROM [symprptprd-reporting].[dbo].[User] U
    LEFT JOIN [dbo].[UserIDs_ORCID] UIO ON U.[ID] = UIO.[User ID] AND UIO.[rn] = 1
    LEFT JOIN [dbo].[UserIDs_ResearcherID] UIR ON U.[ID] = UIR.[User ID] AND UIR.[rn] = 1
    LEFT JOIN [dbo].[UserIDs_Scopus] UIS ON U.[ID] = UIS.[User ID] AND UIS.[rn] = 1
    LEFT JOIN [LTUCWBolitho].LTUUsersDeptsSchoolsColleges AS LUDSC ON U.[ID] = LUDSC.[User_ID]
    LEFT JOIN [LTUCWBolitho].LTUDeptsSchoolsColleges AS LDSC ON U.[Department] = LDSC.[Department]