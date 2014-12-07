rd(customer, {name, uuid, creation_time}).
rd(device, {name, uuid, creation_time, update_time}).
rd(package, {name, uuid, creation_time, update_time, latest_release}).
rd(release, {name, uuid, creation_time, version, files}).

fw:customer_add(#customer{name = acme3}).

# Device
fw:device_add(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{name=dev1}).
fw:device_get(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}).
# Using gen_server
rd(tt_device, {name, id, customer, description}).
ttdb:device_add(#tt_device{name = somerandom, id=asdx00e,customer = "773b00e3-8363-4ff5-ae99-ae160dbed1bd", description = "rather lengthy"}).

# Package
fw:package_add(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{uuid="206f657b-8fe2-4af8-b0c0-c1ccdb9cc5ab"}, #package{name=acmepkg}.
fw:package_get(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{uuid="206f657b-8fe2-4af8-b0c0-c1ccdb9cc5ab"}).

# Release
R = #release{files = ["/foo/bar1", "/bar/baz2"], version=1, name="sandy"}.
fw:release_add(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{uuid="206f657b-8fe2-4af8-b0c0-c1ccdb9cc5ab"}, #package{uuid="e8ab741f-da19-4935-95ef-c07b020d92c8"}, R).
fw:release_get(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{uuid="206f657b-8fe2-4af8-b0c0-c1ccdb9cc5ab"}, #package{uuid="e8ab741f-da19-4935-95ef-c07b020d92c8"}).



