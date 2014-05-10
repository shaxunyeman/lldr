{application,lldr,
  [{description,"A cloud disk developed by db.liu"},
	{vsn,"1.0.0"},
	{modules,[handle_protocol,lldr_data_store,lldr_handle_pack,lldr_handle_server,
			  lldr_protocol_json,lldr_response,lldr_server_sup,lldr_socket_server,
			  lldr_sup,mochijson2,lldr_echo_server,lldr_app]},
	{registered,[lldr_sup,lldr_server_sup,lldr_data_store,lldr_handle_server,lldr_echo_server]},
	{applications,[kernel,stdlib]},
	{env,[{root_data_path,"E:/work/lldr/data/"}]},
	{mod,{lldr_app,[{file,"./lldr_sup.spec"}]}}
	]
}.
