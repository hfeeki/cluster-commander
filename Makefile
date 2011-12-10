BEAM_DIR=./ebin
SRC_DIR=./src
INCLUDE_DIR=./include


compile:
	mkdir -p $(BEAM_DIR)
	erlc -I $(INCLUDE_DIR) -o $(BEAM_DIR) $(SRC_DIR)/*.erl


clean:
	rm -f $(BEAM_DIR)/*.beam
	rm -f $(BEAM_DIR)/erl_crash.dump
