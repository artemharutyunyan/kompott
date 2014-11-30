rd(customer, {name, uuid, creation_time}).
rd(device, {name, uuid, creation_time, update_time}).
rd(package, {name, uuid, creation_time, update_time, latest_release}).
rd(release, {name, uuid, creation_time, version, files}).

fw:customer_add(#customer{name = acme3}).

fw:device_add(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{name=dev1}).

fw:device_get(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}).

