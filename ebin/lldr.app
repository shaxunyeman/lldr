{application,lldr
  [{description,"A cloud disk"},
	{vsn,"1.0.0"},
	{modules,[server,protocol,response,handle_protocol,lldr_data_store]},
	{registered,[lldr_data_store]},
	{mod,[kernel,stdlib]}
	]
}.
