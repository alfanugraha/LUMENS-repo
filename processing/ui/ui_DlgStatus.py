# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file '/home/alex/devel/cpp/qgis/python/plugins/processing/ui/DlgResults.ui'
#
# Created: Tue Oct 29 11:47:17 2013
#      by: PyQt4 UI code generator 4.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_DlgStatus(object):
    def setupUi(self, DlgStatus):
        DlgStatus.setObjectName(_fromUtf8("DlgStatus"))
        DlgStatus.resize(289, 438)
        self.dockWidgetContents = QtGui.QWidget()
        self.dockWidgetContents.setObjectName(_fromUtf8("dockWidgetContents"))
        self.verticalLayout1 = QtGui.QVBoxLayout(self.dockWidgetContents)
        self.verticalLayout1.setObjectName(_fromUtf8("verticalLayout1"))
        self.verticalLayout2 = QtGui.QVBoxLayout()
        self.verticalLayout2.setObjectName(_fromUtf8("verticalLayout2"))
        self.webView = QtWebKit.QWebView(self.dockWidgetContents)
        self.webView.setUrl(QtCore.QUrl(_fromUtf8("about:blank")))
        self.webView.setObjectName(_fromUtf8("webView"))
        self.verticalLayout2.addWidget(self.webView)
        self.verticalLayout1.addLayout(self.verticalLayout2)
        DlgStatus.setWidget(self.dockWidgetContents)

        self.retranslateUi(DlgStatus)
        QtCore.QMetaObject.connectSlotsByName(DlgStatus)

    def retranslateUi(self, DlgStatus):
        DlgStatus.setWindowTitle(QtGui.QApplication.translate("DlgResults", "Status LUMENS Database", None, QtGui.QApplication.UnicodeUTF8))


from PyQt4 import QtWebKit
