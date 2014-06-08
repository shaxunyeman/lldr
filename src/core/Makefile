
INCLUDE = ../..
OUTPUT = ../../ebin
PA = "C:/Program Files/erl5.10.4/lib/erts-5.10.4/ebin"

BEAMS = $(OUTPUT)/loglevel.beam $(OUTPUT)/tcp_listener.beam $(OUTPUT)/tcp_server_app.beam \
		$(OUTPUT)/tcp_echo_fsm.beam

FLAGS = -Werror -W1 -v


ALL : $(BEAMS)
  
$(OUTPUT)/loglevel.beam : loglevel.erl
	erlc -I $(INCLUDE) -o $(OUTPUT) $(FLAGS) loglevel.erl

$(OUTPUT)/tcp_listener.beam : tcp_listener.erl
	erlc -I $(INCLUDE) -o $(OUTPUT) $(FLAGS) -pa $(PA) tcp_listener.erl

$(OUTPUT)/tcp_server_app.beam : tcp_server_app.erl
	erlc -I $(INCLUDE) -o $(OUTPUT) $(FLAGS) tcp_server_app.erl

$(OUTPUT)/tcp_echo_fsm.beam : tcp_echo_fsm.erl
	erlc -I $(INCLUDE) -o $(OUTPUT) $(FLAGS) tcp_echo_fsm.erl

clean:
	rm $(BEAMS)
