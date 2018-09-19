Firebase Cloud Messaging
========================

* FCM application provides push notificaitons to Mobile App.
    * Add firebase api key to your **sys.config** file.
    * Run the rebar shell and excute below command
   
```bash
    fcm:push(#{to => APPToken , notification => Payload})
```

Build
-----
Compiles the Source files

    $ rebar3 compile 

Directs to Erlang shell

    $ rebar3 shell

 Runs the test cases

    $ rebar3 ct 
