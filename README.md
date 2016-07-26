#Commander

The overall structure of the project is like this:

```
src
├── Commander    
│   ├── Conf.hs               -- Read in configuration
│   ├── EC2
│   │   └── SecurityGroup.hs  -- For creating necessary security groups
│   ├── EC2.hs                -- Automations for setting up and tearing down instances
│   ├── Network.hs            -- Streaming from instances to files
│   ├── S3.hs                 -- For uploading scripts to s3
│   ├── Types.hs              -- Collection of types used globally in the project
│   └── Utils.hs              -- Helper functions
├── Commander.hs              -- Main logic path
└── macros.h                  -- CPP Macros for logging. Generates Template Haskell.
```


First of all, a configuration is loaded from one of the following directories in the respective order:
    
```
  ./commander.conf
  $HOME/.commander.conf
  /etc/commander/conf
```


Then, any scripts found within the `scripts` directory are copied into the designated s3 bucket.

After that, based on your configuration, ec2 instances are spun up and assigned to the correct parameters defined within the configuration.
The number of instances is determined by the number of scripts found within the `scripts` directory. Each host is assigned it's own script and is given a personalized UserData parameter on launch that will download the specified script and run it on the host.

After the hosts have come online, Commander waits until all hosts are initialized and then connects to them on a specified port. Once connected each host will run it's designated script and pipe the output back through the socket where it will be dumped into a local log file.

Once all sockets are disconnected, Commander will destroy all of the ec2 instances automatically.
