OUTPUT_DIR=docs/js
TARGET=${OUTPUT_DIR}/Visualizer.min.js

${TARGET}: ${OUTPUT_DIR}
	$(MAKE) -C front
	mv front/build/*.js ${OUTPUT_DIR}


${OUTPUT_DIR}:
	mkdir -p $@