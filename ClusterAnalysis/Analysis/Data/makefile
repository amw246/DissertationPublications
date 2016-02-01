###################
# Makefile for Cluster Analysis from Dissertation
# Andrew Wallace
# Updated 30 Jan 2016
###################

# Key variables to define
RDIR = .
MERGE_OUT = MergeData.Rout

# Create list of R source files
RSOURCE = $(wildcard $(RDIR/*.R))

# Files to indicate when the RSOURCE file was run
OUT_FILES = $(RSOURCE: .R=.Rout)

# Default target
all: $(OUT_FILES)

# Run the RSOURCE files
$(RDIR)/%.Rout: $(RDIR)/%.R
	R CMD BATCHE $<
	
# Remove Out Files
clean:
	rm -fv $(OUT_FILES)
	
# Remove MergeData.Rout
cleanMerge:
	rm -fv $(MERGE_OUT)
	
	
# The correct order is 
# GatherData
# CodeStateVars
# PruneSample
# DefineFactors
# SaveData
# CreateStateSeqObj
# CreateTopTwentyFigures