# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'ui/neuralnetworkwidgetbase.ui'
#
# Created: Fri Oct 18 13:30:57 2013
#      by: PyQt4 UI code generator 4.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class Ui_Widget(object):
    def setupUi(self, Widget):
        Widget.setObjectName(_fromUtf8("Widget"))
        Widget.resize(660, 449)
        Widget.setWindowTitle(_fromUtf8(""))
        self.gridLayout_2 = QtGui.QGridLayout(Widget)
        self.gridLayout_2.setObjectName(_fromUtf8("gridLayout_2"))
        spacerItem = QtGui.QSpacerItem(20, 77, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.gridLayout_2.addItem(spacerItem, 1, 0, 1, 1)
        self.gridLayout = QtGui.QGridLayout()
        self.gridLayout.setObjectName(_fromUtf8("gridLayout"))
        self.leValidationError = QtGui.QLineEdit(Widget)
        self.leValidationError.setReadOnly(True)
        self.leValidationError.setObjectName(_fromUtf8("leValidationError"))
        self.gridLayout.addWidget(self.leValidationError, 6, 1, 1, 2)
        self.label_9 = QtGui.QLabel(Widget)
        self.label_9.setObjectName(_fromUtf8("label_9"))
        self.gridLayout.addWidget(self.label_9, 4, 0, 1, 1)
        self.label_2 = QtGui.QLabel(Widget)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.gridLayout.addWidget(self.label_2, 1, 0, 1, 1)
        self.label = QtGui.QLabel(Widget)
        self.label.setObjectName(_fromUtf8("label"))
        self.gridLayout.addWidget(self.label, 0, 0, 1, 1)
        self.label_10 = QtGui.QLabel(Widget)
        self.label_10.setObjectName(_fromUtf8("label_10"))
        self.gridLayout.addWidget(self.label_10, 6, 0, 1, 1)
        self.label_5 = QtGui.QLabel(Widget)
        self.label_5.setObjectName(_fromUtf8("label_5"))
        self.gridLayout.addWidget(self.label_5, 3, 0, 1, 1)
        self.spnMaxIterations = QtGui.QSpinBox(Widget)
        self.spnMaxIterations.setMinimum(1)
        self.spnMaxIterations.setMaximum(1000000)
        self.spnMaxIterations.setSingleStep(10)
        self.spnMaxIterations.setProperty("value", 500)
        self.spnMaxIterations.setObjectName(_fromUtf8("spnMaxIterations"))
        self.gridLayout.addWidget(self.spnMaxIterations, 2, 1, 1, 2)
        self.spnNeigbourhood = QtGui.QSpinBox(Widget)
        self.spnNeigbourhood.setMinimum(0)
        self.spnNeigbourhood.setMaximum(99)
        self.spnNeigbourhood.setObjectName(_fromUtf8("spnNeigbourhood"))
        self.gridLayout.addWidget(self.spnNeigbourhood, 0, 1, 1, 2)
        self.spnMomentum = QtGui.QDoubleSpinBox(Widget)
        self.spnMomentum.setDecimals(3)
        self.spnMomentum.setMinimum(0.001)
        self.spnMomentum.setMaximum(1.0)
        self.spnMomentum.setSingleStep(0.005)
        self.spnMomentum.setProperty("value", 0.05)
        self.spnMomentum.setObjectName(_fromUtf8("spnMomentum"))
        self.gridLayout.addWidget(self.spnMomentum, 4, 1, 1, 2)
        self.label_3 = QtGui.QLabel(Widget)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.gridLayout.addWidget(self.label_3, 2, 0, 1, 1)
        self.leDeltaRMS = QtGui.QLineEdit(Widget)
        self.leDeltaRMS.setReadOnly(True)
        self.leDeltaRMS.setObjectName(_fromUtf8("leDeltaRMS"))
        self.gridLayout.addWidget(self.leDeltaRMS, 5, 1, 1, 2)
        self.leTopology = QtGui.QLineEdit(Widget)
        self.leTopology.setObjectName(_fromUtf8("leTopology"))
        self.gridLayout.addWidget(self.leTopology, 3, 1, 1, 2)
        self.spnLearnRate = QtGui.QDoubleSpinBox(Widget)
        self.spnLearnRate.setDecimals(3)
        self.spnLearnRate.setMinimum(0.001)
        self.spnLearnRate.setMaximum(1.0)
        self.spnLearnRate.setSingleStep(0.005)
        self.spnLearnRate.setProperty("value", 0.02)
        self.spnLearnRate.setObjectName(_fromUtf8("spnLearnRate"))
        self.gridLayout.addWidget(self.spnLearnRate, 1, 1, 1, 2)
        self.leKappa = QtGui.QLineEdit(Widget)
        self.leKappa.setReadOnly(True)
        self.leKappa.setObjectName(_fromUtf8("leKappa"))
        self.gridLayout.addWidget(self.leKappa, 7, 1, 1, 1)
        self.label_6 = QtGui.QLabel(Widget)
        self.label_6.setObjectName(_fromUtf8("label_6"))
        self.gridLayout.addWidget(self.label_6, 7, 0, 1, 1)
        self.label_4 = QtGui.QLabel(Widget)
        self.label_4.setObjectName(_fromUtf8("label_4"))
        self.gridLayout.addWidget(self.label_4, 5, 0, 1, 1)
        self.btnStop = QtGui.QPushButton(Widget)
        self.btnStop.setObjectName(_fromUtf8("btnStop"))
        self.gridLayout.addWidget(self.btnStop, 10, 1, 1, 1)
        self.btnTrainNetwork = QtGui.QPushButton(Widget)
        self.btnTrainNetwork.setObjectName(_fromUtf8("btnTrainNetwork"))
        self.gridLayout.addWidget(self.btnTrainNetwork, 10, 0, 1, 1)
        self.gridLayout.setColumnStretch(0, 2)
        self.gridLayout.setColumnStretch(1, 1)
        self.gridLayout_2.addLayout(self.gridLayout, 0, 0, 1, 1)
        self.layoutPlot = QtGui.QVBoxLayout()
        self.layoutPlot.setObjectName(_fromUtf8("layoutPlot"))
        self.gridLayout_2.addLayout(self.layoutPlot, 0, 1, 2, 1)

        self.retranslateUi(Widget)
        QtCore.QMetaObject.connectSlotsByName(Widget)

    def retranslateUi(self, Widget):
        self.label_9.setText(QtGui.QApplication.translate("Widget", "Momentum", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("Widget", "Learning rate", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("Widget", "Neighbourhood", None, QtGui.QApplication.UnicodeUTF8))
        self.label_10.setText(QtGui.QApplication.translate("Widget", "Min Validation Overall Error", None, QtGui.QApplication.UnicodeUTF8))
        self.label_5.setText(QtGui.QApplication.translate("Widget", "Hidden Layers", None, QtGui.QApplication.UnicodeUTF8))
        self.spnNeigbourhood.setSuffix(QtGui.QApplication.translate("Widget", " px", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("Widget", "Maximum iterations", None, QtGui.QApplication.UnicodeUTF8))
        self.label_6.setText(QtGui.QApplication.translate("Widget", "Current Validation Kappa", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("Widget", "Δ Overall Accuracy", None, QtGui.QApplication.UnicodeUTF8))
        self.btnStop.setText(QtGui.QApplication.translate("Widget", "Stop", None, QtGui.QApplication.UnicodeUTF8))
        self.btnTrainNetwork.setText(QtGui.QApplication.translate("Widget", "Train neural network", None, QtGui.QApplication.UnicodeUTF8))

