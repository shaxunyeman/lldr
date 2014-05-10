1.build
	1.1 erl
	1.2 c(build)
	1.3 build:start()
	All objects will be compiled into ebin


2.supervisor	
					------------
					| lldr_sup |
					------------
						 |
						 |
		_______________________________	  
		|				  |
		|				  |
 lldr_data_store	lldr_server_sup
						  |
						  |
				  _______________
				  |				|
				  |				|
				echosrv	  lldr_handle_server	
