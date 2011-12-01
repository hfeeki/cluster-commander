BEAM_DIR=./ebin
SRC_DIR=./src


compile:
	mkdir -p $(BEAM_DIR)
	erlc -o $(BEAM_DIR) $(SRC_DIR)/*


clean:
	rm -f $(BEAM_DIR)/*.beam
	rm -f $(BEAM_DIR)/erl_crash.dump
