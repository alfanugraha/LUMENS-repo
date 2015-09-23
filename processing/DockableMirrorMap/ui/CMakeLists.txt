FILE(GLOB PY_FILES *.py)

PLUGIN_INSTALL(processing ./DockableMirrorMap/ui ${PY_FILES})
