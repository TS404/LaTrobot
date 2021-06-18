# LaTrobot
A set of scripts in R to disambiguate and add open data from La Trobe University to Wikidata

## Inputs
Uses and SQL query of La Trobe Uni's databases to collate non-sensitive data that is already public or could be made so.

For current and past academic staff (and published students with ORCIDs):
-	Department affiliation, Position, Employment dates
-	Fields of expertise (as semantic concepts, not plaintext)
-	Links to LTU website address, ORCID, SCOPUSID, ResearchID
-	References to support for the above (typically ltu profile page as appropriate)

## Process
•	Finds unassociated ORCiDs based on name and affiliation (side benefit!)
•	Finds existing Wikidata items for academics based on ORCIDs, ScopusIDs and ResearchIDs (and names if necessary with manual disambiguation)
•	Finds and disambiguates Wikidata semantic items for fields expertise and positions

## Outputs
•	Updates existing Wikidata items
•	Creates missing Wikidata items for those not on WD

# Acknowledgements
Inspired by the conceptual framework of [VanderBot](https://github.com/HeardLibrary/linked-data/tree/master/vanderbot) ([Steve Baskauf](https://github.com/baskaufs)) but implemented in R rather than Python.
