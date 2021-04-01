

stim_stuff/stimlist_retrieval_%.csv: R/make_spreadsheet_retrieval_%.R \
								   	 stim_stuff/master_spreadsheet_parsed.csv
	Rscript -e 'source("$<")'

stim_stuff/stimlist_encoding_firsthalf.csv \
stim_stuff/stimlist_encoding_secondhalf.csv: R/make_spreadsheet_encoding.R \
											 stim_stuff/master_spreadsheet_parsed.csv
	Rscript -e 'source("$<")'

stim_stuff/stimlist_pretest_*.csv: R/make_spreadsheet_pretest.R \
								   stim_stuff/master_spreadsheet.csv
	Rscript -e 'source("$<")'

stim_stuff/master_spreadsheet_parsed.csv: R/get_master_spreadsheet.R \
										  ignore/narration/durations_parsed.csv
	Rscript -e 'source("$<"); parse_master_spreadsheet()'

ignore/narration/durations_parsed.csv: R/parse_narr_durations.R ignore/narration/durations.txt

ignore/narration/durations.txt: shell/get_narr_durations.sh ignore/narration/renamed/*
	$<

# master_spreadsheet.csv does NOT have a dependency
# managed manually, bc this doesn't really slickly track gdrive edits
