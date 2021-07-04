# Collates user expertise from symprptprd and concatenates them into a single data field using a semicolon as the delimiter

SELECT DISTINCT
    U.[ID] AS [User ID],
    U.[Computed Name Alphabetical],
    (SELECT
    STUFF(
    (SELECT
    CONCAT('; ', UL.[Label])
    FROM [symprptprd-reporting].[dbo].[User Label] UL 
    WHERE UL.[Scheme ID] = 9 AND UL.[User ID] = U.[ID]
    FOR XML PATH(''),TYPE).value('.', 'varchar(max)'), 1, 2, '')
    ) AS 'Expertise'

 

FROM  [symprptprd-reporting].[dbo].[User] U