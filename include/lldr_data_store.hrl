-record(user,{
		mail,		  %% email of user,this is uniqueue
		sex,		  %% sex of user
		name,		  %% name of user
		path		  %% path of data that user owns
		}).


-record(password,{
		mail,		  %% email of user,this is uniqueue
		string		  %% password of user
		}).

