# -*- coding: utf-8 -*-

"""
***************************************************************************
    ResultsDialog.py
    ---------------------
    Date                 : August 2012
    Copyright            : (C) 2012 by Victor Olaya
    Email                : volayaf at gmail dot com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
***************************************************************************
"""

from PyQt4.QtCore import *
from PyQt4.QtGui import *

from processing.ui.ui_DlgStatus import Ui_DlgStatus


class StatusDatabase(QDockWidget, Ui_DlgStatus):

    def __init__(self):
        QDockWidget.__init__(self, None)
        self.setupUi(self)
        self.setAllowedAreas(Qt.LeftDockWidgetArea | Qt.RightDockWidgetArea)
        
    def setStatusDatabase(self, filename):
        self.webView.load(QUrl(filename))

