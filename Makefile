.PHONY: clean clean-test clean-simu simu

clean:
	rm -f ./generated/*

clean-test:
	rm -f -r ./test_run_dir/*

clean-simu:
	rm -f -r ./simu-out/*

simu: simu-out/latency.png

simu-out/latency.png: simu-out/latency.csv
	python draw.py

simu-out/latency.csv:
	sbt 'testOnly mesh_network.NetworkSpec'