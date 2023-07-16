# kuberlnetes

An rather low-level OTP library for interacting with the Kubernetes API server. 
Primarly used in applications that interact with Kubernetes from within the cluster.

## Why?

The primary goal `kuberlnetes` is not to create a high-level abstraction over 
the Kubernetes API but provide simple functions that simplify certain operations and make 
them easily available for Erlang applications.

There is no generated code or any forced model - only supporting functions for the well-known
REST operations. This eases backward and forward compatibility (at least for `kuberlnetes` itself).

## Basic Examples

`kuberlnetes` returns the plain responses as defined in the [Kubernetes API specification](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.26/) as Erlang
maps.

> Bear in mind that `get | post | patch | put` functions are blocking!

Applications accessing the Kubernetes API from within the cluster:

```erlang
Server = kuberlnetes:in_cluster(),
{ok, DeploymentsList} = kuberlnetes:get("/apis/apps/v1/deployments"),
#{<<"items">> := Deployments} = DeploymentsList,
...
```

Access the Kubernetes API from outside the cluster using a local 
`~/.kube/config` file:

```erlang
Server = kuberlnetes:from_config(#{context => "some_context}}),
...
```

## Watches

`kuberlnetes` supports the [Watch API](https://kubernetes.io/docs/reference/using-api/api-concepts/#efficient-detection-of-changes):

```erlang
Server = kuberlnetes:from_config(#{context => "some_context"}),
{ok, WatchPid} = kuberlnetes:watch(#{
        server => Server,
        api_group => "/api/v1",
        kind => "configmaps",
        name => "my-config",
        namespace => "default"
    }).
```

Each open watch is represented by an erlang process. By default messages change messages are sent to the process that spawned the watch proce
The received message has the following format:

```erlang
{kuberlnetes_watch, added, #{kind => <<"Pod">>, ...}} | 
{kuberlnetes_watch, modified, #{...}} | 
{kuberlnetes_watch, deleted, #{...}}.

```

## Build 

```
$ make
```
