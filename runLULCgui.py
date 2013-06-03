import sys, os
import shutil
import os.path
from os.path import basename, splitext

#################QT GUI IMPORTS#################
from PyQt4 import QtCore, QtGui
from PyQt4.QtCore import Qt
from PyQt4.QtCore import pyqtSignature
from LULCgui import Ui_LULCModel

#################FOR R IMPORT####################
import rpy2.robjects as R
from rpy2.robjects.packages import importr 


 
class MyForm(QtGui.QMainWindow):


  
  
  ###############################333
  #Add all the save variable here
  __projectDirectory = "."
  __text = ""
  __raster = True
  __currentDirectory="."
  __layerT0 = ""
  __layerT1 = ""
  __shpfileT0 = ""
  __shpfileT1 = ""

  def r_converToRaster():
    rvalue = R.r['pi']
    QtGui.QMessageBox.about(self, "My message box", "rvalue = %s" % (rvalue));
        
  def __init__(self, parent=None):
    R.r(''' source('Rasterise_dev_6.R') ''')
    QtGui.QWidget.__init__(self, parent)
    self.ui = Ui_LULCModel()
    self.ui.setupUi(self)
    #self.setGeometry(100,60,900,700)
    self.setWindowTitle("Project IIRS")
    #self.setWindowIcon(QtGui.QIcon('icons/name.png'))
    #All the new objects here as self.ui.

   ##################################
  #Adding icon to the project
  
    
    
    #Add all the requuired signles Here
  @pyqtSignature("")
  def on_pbSelectDirectory_clicked(self):
        self.__projectDirectory = QtGui.QFileDialog.getExistingDirectory(self, "Open Project Directory",
                            ".", QtGui.QFileDialog.ShowDirsOnly);
        self.ui.leProjectDirectory.setText(str(self.__projectDirectory))
        #QtCore.QObject.connect(self.ui.pbSelectDirectory, QtCore.SIGNAL("clicked()"), self.ui.leProjectDirectory.clear )
        #QtCore.QObject.connect(self.ui.lineEdit, QtCore.SIGNAL("returnPressed()"), self.add_entry)
 
  @pyqtSignature("")
  def on_pbSelectFile_T0_clicked(self):
        file = QtGui.QFileDialog.getOpenFileName(self, "Open File",
                            self.__currentDirectory,"Raster (*.tiff *.tif *.shp)");
        (dirName, fileName) = os.path.split(str(file))
        self.__currentDirectory=dirName
        self.__layerT0=splitext(fileName)[0]
        self.__shpfileT0 = str(file)
        self.ui.leT0File.setText(str(file))

  @pyqtSignature("")
  def on_pbConvert_T0_clicked(self):
      r_rasterize = R.globalenv['rasterise']
      r_rasterize(self.__shpfileT0,self.__layerT0,self.ui.sbGridsize.value())
      self.ui.leOutputFile.setText(str(self.__currentDirectory)+"/"+self.__layerT0+".tif")

  @pyqtSignature("")
  def on_pbSelectFile_T1_clicked(self):
        file = QtGui.QFileDialog.getOpenFileName(self, "Open File",
                            self.__currentDirectory,"Raster (*.tiff *.tif *.shp)");
        (dirName, fileName) = os.path.split(str(file))
        self.__currentDirectory=dirName
        self.__layerT1=splitext(fileName)[0]
        self.__shpfileT1 = str(file)
        self.ui.leT1File.setText(str(file))
  
  @pyqtSignature("")
  def on_pbConvert_T1_clicked(self):
      r_rasterize = R.globalenv['rasterise']
      r_rasterize(self.__shpfileT1,self.__layerT1,self.ui.sbGridsize.value())

  @pyqtSignature("")

  def on_pbSelectFile_Mask_clicked(self):

        file = QtGui.QFileDialog.getOpenFileName(self, "Open File",

                            self.__currentDirectory,"Raster (*.tiff *.tif )");

        (dirName, fileName) = os.path.split(str(file))

        self.__currentDirectory=dirName

        self.ui.leMaskFile.setText(str(file))
    

  

  @pyqtSignature("")
  def on_rbRasterFile_clicked(self):
    if (self.__raster==False):
        self.ui.rbVectorFile.setChecked(False)
        self.ui.pbConvertToRaster_T0.setEnabled(False)
        self.ui.pbConvertToRaster_T1.setEnabled(False)
        self.ui.rbRasterFile.setChecked(True)
        self.__raster=True


  @pyqtSignature("")
  def on_rbVectorFile_clicked(self):
        if (self.__raster):
            self.ui.rbVectorFile.setChecked(True)
            self.ui.pbConvertToRaster_T0.setEnabled(True)
            self.ui.pbConvertToRaster_T1.setEnabled(True)
            self.ui.rbRasterFile.setChecked(False)
            self.__raster=False

  def getCurrentDirectory():
        return(self.__currentDirectory)
        

  @pyqtSignature("")
  def on_pbSelectFile_OutputFile_clicked(self):
        """dir = QtGui.QFileDialog.getSaveFileName(self, "Output File",
                            self.__projectDirectory,"*.tif");
        
        self.ui.leOutputFile.setText(str(dir))
        file = QtGui.QFileDialog.getOpenFileName(self, "Output File",
                            self.__projectDirectory,"Raster (*.tiff *.tif )");
        (dirName, fileName) = os.path.split(str(file))
        self.__currentDirectory=dirName
        self.ui.leOutputFile.setText(str(file))"""
        shutil.move(self.__layerT0+".tif",str(self.__projectDirectory))


  @pyqtSignature("")
  def on_pbConvertToRaster_T0_clicked(self):
        if (self.__raster==False):
            self.r_convertToRaster();

  @pyqtSignature("")
  def on_pbNextDataPreparation_clicked(self):
    self.ui.tabWidget.setCurrentIndex(1)
    self.ui.progressBar.setProperty("value",10)
  @pyqtSignature("")
  def on_pushButton_19_clicked(self):
    self.ui.tabWidget.setCurrentIndex(2)
    self.ui.progressBar.setProperty("value",20)
  
    
#################Module:2####################

  @pyqtSignature("")
  def on_pbAddDriver_clicked(self):
    self.ui.tableWidget.setRowCount(self.ui.tableWidget.rowCount() + 1)
  
 
if __name__ == "__main__":
  app = QtGui.QApplication(sys.argv)
  myapp = MyForm()
  myapp.show()
  sys.exit(app.exec_())
