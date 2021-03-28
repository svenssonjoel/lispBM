
all:
	west build -p auto -b nrf52840dk_nrf52840 .

dev_kit:
	west build -p auto -b nrf52840dk_nrf52840 .

nrf_mod:
	west build -p auto -b nrf_module_v1 -- -DBOARD_ROOT=../../LLBridge .

clean:
	rm -r build/
