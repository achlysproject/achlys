# Achlys
The Achlys framework is a tool designed to help application developers build Erlang/OTP programs using the [Lasp](https://lasp-lang.readme.io) libraries and running on [GRiSP](https://grisp.org) embedded systems in a wireless sensor network configuration.

Achlys is being developed in the context of edge computing research within the H2020 [LightKone](https://lightkone.eu) project. The 2 main objectives of the framework are to provide :

- Resilient storage across a cluster of IoT sensing devices
- A general purpose task model allowing any function to be propagated and executed in the cluster

Disclaimer : Achlys is currently under active development, hence a production-ready release is not yet available.

> Achlys is the goddess of deadly poison ... but fortunately there is an [AntidoteDB](https://www.antidotedb.eu/)

## Minimum requirements

- [otp](https://github.com/erlang/otp) `21.0.9`
- [rebar3](https://github.com/erlang/rebar3) `3.6.2`
- [grisp](https://github.com/grisp/grisp) `1.1.4`
- [rebar3_grisp](https://github.com/grisp/rebar3_grisp) `1.2.3`
- [grisp-software](https://github.com/grisp/grisp-software) : a fully built grisp toolchain.
- [partisan](https://github.com/lasp-lang/partisan) `3.0.0`
- [lasp](https://github.com/lasp-lang/lasp) `0.9.0`

## Wiki

A [Wiki](https://github.com/Laymer/achlys/wiki/Achlys-Wiki) is currently being written and will aim at providing a wide
range of examples and tutorials in order to demonstrate the capabilities of Lasp on GRiSP at the Edge.

## EDoc preview

The development process has not yet reached a level of maturity allowing for an actual release of the program.
Therefore, the documentation is currently more of an insight at the software design and more generally at
the features that will be provided.

Once a satisfactory amount of testing and features will be implemented, a versioned package will be released and the documentation will be a reliable set of specifications.

The online version of the documentation is found at :

[HexDocs](https://hexdocs.pm/achlys)

### Architecture

The design pattern will follow the "facade" concept as much as possible, hence there will be an API that will provide an easy access to all the modules and functions. The documentation will be reorganized to focus on thoroughly explaining the usage, and will regroup the information of submodules like these :

<p align="center">
  <img src="resources/Doc_preview.png" alt="EDoc"/>
</p>


## Mind map

A [Mindly](http://www.mindlyapp.com) reasoning construct.
Allows for easier visualization of problems and tasks, hence more efficient solving.
The **API** branch of the map provides a description of some features and simple usage examples.
For Achlys, it is currently an additional asset to structure software improvement ideas.

### NOTE : An online interactive version is available [here](https://laymer.github.io/achlys-map/)

<p align="center">
  <img src="resources/Achlys.png" alt="MMap"/>
</p>
