{
  {one_for_one,1,5},
  [
	{lldr_server_sup,{lldr_server_sup,start_link,[]},transient,5000,supervisor,[lldr_server_sup]},
	{lldr_data_store,{lldr_data_store,start_link,[]},transient,5000,worker,[lldr_data_store]}
  ]
}.
