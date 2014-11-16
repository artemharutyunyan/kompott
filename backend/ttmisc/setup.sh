#!/bin/bash

## Create and activate bucket types for sets
riak-admin bucket-type create sets '{"props": {"datatype": "set"}}'
riak-admin bucket-type activate sets

