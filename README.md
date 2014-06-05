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

3. How to run lldr
  3.1 erl -mnesia dir '"E:/work/lldr/mnesia/data"'
  3.2. start application
		application:start(lldr).

4. How to stop application
  application:stop(lldr).
