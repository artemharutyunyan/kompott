rd(customer, {name, uuid, creation_time}).
rd(device, {name, uuid, customer_id, external_id, description, creation_time, update_time}).
rd(package, {name, uuid, creation_time, update_time, latest_release}).
rd(release, {name, uuid, creation_time, version, files}).

rd(tt_device, {name, id, customer, description}).

ttdb:device_get(#tt_device{customer = "773b00e3-8363-4ff5-ae99-ae160dbed1bd"}).

fw:customer_add(#customer{name = acme3}).

# Device
fw:device_add(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{name=dev1}).
fw:device_get(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}).

# Package
fw:package_get(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{uuid="206f657b-8fe2-4af8-b0c0-c1ccdb9cc5ab"}).

# Release
R = #release{files = ["/foo/bar1", "/bar/baz2"], version=1, name="sandy"}.
fw:release_add(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{uuid="206f657b-8fe2-4af8-b0c0-c1ccdb9cc5ab"}, #package{uuid="e8ab741f-da19-4935-95ef-c07b020d92c8"}, R).
fw:release_get(#customer{uuid="773b00e3-8363-4ff5-ae99-ae160dbed1bd"}, #device{uuid="206f657b-8fe2-4af8-b0c0-c1ccdb9cc5ab"}, #package{uuid="e8ab741f-da19-4935-95ef-c07b020d92c8"}).



%% --------------------------------------------------------------------------
%% Public interface (using gen_server)
%% --------------------------------------------------------------------------

%% Records
rd(tt_customer, {name}).
rd(tt_device, {name, id, customer, description}).
rd(tt_package, {name, device, customer, description, id}).

ttdb:customer_add(#tt_customer{name="acme"}).

ttdb:device_add(#tt_device{name = somerandom, id="0xA", customer="60a8c12f-9628-430d-801a-2d95e1d68d8e", description="device description goes here"}).
ttdb:device_get(#tt_device{customer="60a8c12f-9628-430d-801a-2d95e1d68d8e"}).
ttdb:device_get(#tt_device{customer="60a8c12f-9628-430d-801a-2d95e1d68d8e", id="S0ME"}).

ttdb:package_add(#tt_package{customer="60a8c12f-9628-430d-801a-2d95e1d68d8e", device="S0ME",description="first package", name=pkg}).
ttdb:package_get(#tt_package{device="S0ME", customer="60a8c12f-9628-430d-801a-2d95e1d68d8e"}).
ttdb:package_get(#tt_package{device="S0ME", customer="60a8c12f-9628-430d-801a-2d95e1d68d8e", id="4a1fac77-800c-4463-bb57-1e2bce177028"}).

curl --header "Content-Type: application/json"   127.0.0.1:8080/v1/customers/60a8c12f-9628-430d-801a-2d95e1d68d8e/deviceTypes/S0ME
curl --header "Content-Type: application/json"   127.0.0.1:8080/v1/customers/60a8c12f-9628-430d-801a-2d95e1d68d8e/deviceTypes

curl --include --request POST --header 'Content-Type: application/json' --data-binary '{ "name": "Charging station.", "description": "No life without it" }' 127.0.0.1:8080/v1/customers/60a8c12f-9628-430d-801a-2d95e1d68d8e/devicetypes/a454

