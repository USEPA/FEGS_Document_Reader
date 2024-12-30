Synonym List Metadata
Synonym_List_NESCS_plustaxa.csv

This file provides the lists of keywords that define the classes and subclasses in the NESCS Plus hierarchy.
The Excel file is for easier editing, but any edits should be saved as .csv prior to running R code.
The .csv file must be save as a .rda file prior to running code using function create_synonym_lists().


Category-	NESCS Plus Category

Class-	NESCS Plus Higher order class of the focal "SubClass" for which keywords are assigned

SubClass-	NESCS Plus Category to which Keywords in columns S1 - S12 are assigned

Tier-	Defines the hierarchy (Tier 1 = Class, Tier 2 = Subclass, Tier 3 = Sub-subclass, etc.)

Word_type-	"Include" words are the primary search terms.  "Near" words must be found in the same sentence as the "Include" word and function as "AND".  "Exclude" words must not be found in the same sentence as the "Include" word and function as "BUT NOT";  multiple search terms can be defined in the same cell using "|" to represent "OR"

S1-S12-	Up to 12 groups of keywords are allowed;   Keyword groups function similarly in terms of their Include, near, and exclude words (to save space and reduce duplication);   For a given "SubClass" a group (e.g., column S1) is read as the set of 3 "include", "near", and "Exclude" words   and is read as "(Include word 1 OR Include word 2...) AND (Near word 1 OR Near word 2...) BUT NOT (Exclude word 1 OR Exclude word 2...)"
