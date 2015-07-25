-record(peer, 
        {id, port, ip, address, isseeder, identifier,key}).

%% State for tracker loop.
-record(state, {storageservice,
                socketservice,
                port}).

