BEGIN MODEL_COMMAND_LINES TABLE
# Single quotes around 'Command=value' are required if the
# command includes one or more spaces, but are optional 
# otherwise
nrow=1 ncol=3  columnlabels
COMMAND                                              PURPOSE         COMMANDID
..\..\test-data-win\data-transient\tc1-fwd-der.bat   Forward&Der     modflow_sen
END MODEL_COMMAND_LINES