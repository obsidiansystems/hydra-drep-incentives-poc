# Hydra DRep Incentives POC

What follows is the initial phase of an experimental proof-of-concept (POC) to explore whether [Hydra](https://github.com/input-output-hk/hydra) payment channels could be utilized to distribute voting incentives for delegated representatives (DReps) participating in Cardano’s developing [decentralized governance system](https://github.com/cardano-foundation/CIPs/tree/master/CIP-1694).

Our findings suggest that using Hydra here could be feasible, practical, and - in the absence of final decisions about incentive models - could help reduce overall uncertainty by acting as a well-understood open-source off-chain agreement between parties and system components in a variety of situations.

## The POC

Currently the POC provides quite a few features to enable testing of governance and the application of Hydra to DRep incentives

- [x] Automatically configure a sanchonet node
- [x] Automatically run and synchronize a sanchonet node
- [x] Create parties as needed
- [x] Provide automated funding of voter addresses through the governance address
- [x] Party addresses have stake keys, hydra keys, and are registered as DReps
- [x] Allow creation of proposals in scenarios where they have already been voted on (this requires 50000 tADA)
- [x] Synthesize and consume the governance state to determine voters and automate pay-out
- [x] Tested Hydra against Sanchonet both pre and post re-spin, on versions 0.16.0 and 0.17.0
- [x] Track the cumulative transaction hashes during a run through of incentive payment detection
- [x] Use a Hydra offline channel to run incentive payment channel mechanism on an L2
- [x] Track the offline channel incentive payments in the L2 Head
- [ ] Move to online Hydra payment channel once [this issue](https://github.com/input-output-hk/hydra/issues/1462) is resolved

### The Experimental Workflow

This POC web application creates a Sanchonet cardano node, then provides 3 voters and one governance address. The voters have been registered as DReps and their tADA will be automatically kept at the minimum amount (550000000) of Lovelace provided the governance address has funds.
This application automatically updates and monitors the state of the chain, the balances of the governance and voters, and the Hydra network. Another important element the application monitors is the governance state on-chain.

When the application is running and you have navigated to the UI page (by default http://localhost:8000) you will be presented with:

- The governance address (to be able to send tADA if you run low)
- A current governance proposal (if available) with the ability to view the URL encoded inside
- A way to create new proposals so that if one isn't available to vote on, you can immediately create one for testing (note this does take about 50000000000 Lovelace, and thus isn't automated to be conscious of those with limited test-net funds)
- The voters and their balances
- A log of transactions that have occurred during the session
- The current state of the Hydra payment channel that contains the governance address and the voters


You can then vote as any one of the voters, assuming they haven't voted already. Pressing the "Vote" button adjacent to any voter's name will cause a vote transaction to be built, signed, and submitted. After the transaction is confirmed, the vote will be reflected in the governance state, and when this has been detected, an amount of reward Lovelace will be sent to the voter responsible (12345678 Lovelace by default).

#### The governance address

This address is a representation of the governance body of cardano, the specifics of the incentive mechanism and fund source are still [being discussed](https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md#drep-incentives).

#### The voter addresses

Each voter in the UI is a registered DRep on the network, and is eligible to receive incentive payments for voting on proposals that are active

#### The Hydra payment channel

Here is where incentive payments are made, they are recorded in the channel, and displayed accordingly

## How to Build & Run Locally

If you want deploy your application locally or test a production-oriented build you can build and deploy the app as described below.

Build the application:

``` sh
nix-build -A exe --no-out-link
```

Copy the result to a new directory, add configuration, and run!

``` sh
mkdir test-app
ln -s $(nix-build -A exe --no-out-link)/* test-app/
cp -r config test-app
(cd test-app && ./backend)
```

## How Run in Development Mode

``` sh
ob run -v 
```

For now, you will require [obelisk](https://github.com/obsidiansystems/obelisk) to be able to run in development mode, and hack on the application.

### Installing Obelisk

1. [Install Nix](https://nixos.org/nix/).
    If you already have Nix installed, make sure you have version 2.0 or higher.  To check your current version, run `nix-env --version`.
1. Set up nix caches
    1. If you are running NixOS, add this to `/etc/nixos/configuration.nix`:
        ```nix
        nix.binaryCaches = [ "https://nixcache.reflex-frp.org" ];
        nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
        ```
        and rebuild your NixOS configuration (e.g. `sudo nixos-rebuild switch`).
    1. If you are using another operating system or Linux distribution, ensure that these lines are present in your Nix configuration file (`/etc/nix/nix.conf` on most systems; [see full list](https://nixos.org/nix/manual/#sec-conf-file)):
        ```nix
        binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org
        binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=
        binary-caches-parallel-connections = 40
        ```
        * If you're on a Linux distribution other than NixOS, enable sandboxing (see these [issue 172](https://github.com/obsidiansystems/obelisk/issues/172#issuecomment-411507818) or [issue 6](https://github.com/obsidiansystems/obelisk/issues/6) if you run into build problems) by adding the following:
          ```nix
          sandbox = true
          ```
          then restart the nix daemon
          ```bash
          sudo systemctl restart nix-daemon
          ```
        * If you're on MacOS, disable sandboxing (there are still some impure dependencies for now) by adding the following:
          ```nix
          sandbox = false
          ```
          then restart the nix daemon
          ```bash
          sudo launchctl stop org.nixos.nix-daemon
          sudo launchctl start org.nixos.nix-daemon
          ```
1. Install obelisk:
   ```bash
   nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command
   ```

## Contributing

Contributions and issue reports are encouraged and appreciated! Refer to the [Contributing](CONTRIBUTING.md) guide for information about getting started.
