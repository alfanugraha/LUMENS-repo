FILE(GLOB PY_FILES *.py)
FILE(GLOB ICON_FILES icons/*.png)

ADD_SUBDIRECTORY(ui)

PLUGIN_INSTALL(processing ./DockableMirrorMap ${PY_FILES})
PLUGIN_INSTALL(processing ./DockableMirrorMap/icons ${ICON_FILES})
