ignore/data/all: ignore/data/q_google_demos.csv \
								 ignore/data/q_posttask.csv \
								 ignore/data/task_retrieval_source.csv \
								 ignore/data/task_retrieval_pics.csv \
								 ignore/data/task_retrieval_facts.csv \
								 ignore/data/task_encoding.csv \
								 ignore/data/demos_ids.csv \
								 ignore/data/q_trivia_demos.csv \
								 ignore/data/task_jeopardy_meta_states.csv \
								 ignore/data/task_jeopardy_meta_descriptions.csv \
								 ignore/data/task_jeopardy_recall.csv
	touch ignore/data/all

# while google spreadsheet aly lab demos do not ACTUALLY depend on gorilla data,
# setting this dependency should trigger the timestamp condition necessary
# for the output demos CSV to appear "out of date"
# bc there is no way for make to know when the upstream google sheet is updated
ignore/data/q_google_demos.csv: R/clean_q_google_demos.R \
																ignore/data/raw/real/*
	Rscript -e 'source("$<")'

ignore/data/q_posttask.csv: R/clean_q_posttask.R \
														ignore/data/raw/real/* \
														R/utils_read_gorilla.R
	Rscript -e 'source("$<")'

ignore/data/task_retrieval_source.csv: R/clean_retrieval_source.R \
																			 ignore/data/task_pretest.csv \
																			 ignore/data/raw/real/* \
																			 R/utils_read_gorilla.R
	Rscript -e 'source("$<")'

ignore/data/task_retrieval_pics.csv: R/clean_retrieval_pics.R \
																		 ignore/data/task_pretest.csv \
																		 ignore/data/raw/real/* \
																		 R/utils_read_gorilla.R
	Rscript -e 'source("$<")'

ignore/data/task_retrieval_facts.csv: R/score_retrieval_facts.R \
																			ignore/data/task_retrieval_facts_unscored.csv
	R


ignore/data/task_retrieval_facts_unscored.csv: R/clean_retrieval_facts.R \
																							 ignore/data/task_pretest.csv \
																							 ignore/data/raw/real/* \
																							 R/utils_read_gorilla.R
	Rscript -e 'source("$<")'

ignore/data/task_encoding.csv: R/clean_encoding.R \
															 ignore/data/task_pretest.csv \
															 ignore/data/raw/real/* \
															 R/utils_read_gorilla.R
	Rscript -e 'source("$<")'

ignore/data/task_pretest.csv: R/clean_pretest.R \
															ignore/data/raw/real/* \
															R/utils_read_gorilla.R
	Rscript -e 'source("$<")'
	
ignore/data/demos_ids.csv: R/clean_demos_ids.R \
																ignore/data/raw/real/* \
																R/utils_read_gorilla.R
	Rscript -e 'source("$<")'

ignore/data/q_trivia_demos.csv: R/clean_q_trivia_demos.R \
																ignore/data/raw/real/* \
																R/utils_read_gorilla.R
	Rscript -e 'source("$<")'

ignore/data/task_jeopardy_meta_states.csv \
ignore/data/task_jeopardy_meta_descriptions.csv: R/clean_metamemory.R \
																								 ignore/data/raw/real/* \
																								 R/utils_read_gorilla.R
	Rscript -e 'source("$<")'

ignore/data/task_jeopardy_recall.csv: R/score_expertise.R \
																			ignore/data/task_jeopardy_recall_unscored.csv
	R

ignore/data/task_jeopardy_recall_unscored.csv: R/clean_expertise.R \
																							 ignore/data/raw/real/* \
																							 R/utils_read_gorilla.R
	Rscript -e 'source("$<")'

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
											stim_stuff/master_spreadsheet.csv \
										  ignore/narration/durations_parsed.csv
	Rscript -e 'source("$<"); parse_master_spreadsheet()'

ignore/narration/durations_parsed.csv: R/parse_narr_durations.R ignore/narration/durations.txt
	Rscript -e 'source("$<")'

ignore/narration/durations.txt: shell/get_narr_durations.sh ignore/narration/renamed/*
	$<

# master_spreadsheet.csv does NOT have a dependency
# managed manually, bc this doesn't really slickly track gdrive edits
