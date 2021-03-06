# Building kompott backend

## Installing 3rdparty dependencies

### Installing erlang

The following commands will install Erlang version r16b03-1 in /opt/erlang/r16b03-1. Before running please substitute USERNAME with your actual username.


``` bash
  $ curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
  $ chmod +x kerl
  $ ./kerl build R16B03-1 r16b03-1
  $ sudo mkdir /opt/erlang
  $ sudo chown USERNAME /opt/erlang
  $ ./kerl install r16b03-1 /opt/erlang/r16b03-1
```

## Setting up build environment and building the release
Activate your Erlang installation by running

```bash
 $ . /opt/erlang/erlang-r16b03-1/activate
```

Change to kompott source root, setup the build environemt, build 3rdparty dependencies and the backend
```bash
  $ cd kompott
  $ . setup
  $ cd deps
  $ make
  $ cd ../backend
  $ make
```

To generate the release

```bash
  $ cd ttnode
  $ rebar clean compile generate
```
