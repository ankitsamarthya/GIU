import sys, os, random, time
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

__consti = 0

class MyThread(QtCore.QThread):
    trigger = QtCore.pyqtSignal(int)

    def __init__(self, parent=None):
        super(MyThread, self).__init__(parent)

    def setup(self, thread_no):
        self.thread_no = thread_no

    def run(self):
        #time.sleep(random.random()*5)   
        self.trigger.emit(self.thread_no)

class MyThread1(QtCore.QThread):
    trigger1 = QtCore.pyqtSignal(int)

    def __init__(self, parent=None):
        super(MyThread1, self).__init__(parent)

    def setup(self, thread_no):
        self.thread_no = thread_no

    def run(self):
        #time.sleep(random.random()*5)   
        self.trigger1.emit(self.thread_no)        
 
class MyForm(QtGui.QMainWindow):
  
  ###############################333
  #Add all the save variable here
  global __consti
  __projectDirectory = "."
  __text = ""
  __raster = True
  __currentDirectory="."
  __layerT0 = ""
  __layerT1 = ""
  __shpfileT0 = ""
  __shpfileT1 = ""
  __check = 1
  

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
    self.ui.leOutputFile.setText("")
    #self.setWindowIcon(QtGui.QIcon('icons/name.png'))
    #All the new objects here as self.ui.

   ##################################
  #Adding icon to the project

  def update_text(self, thread_no):
        QtGui.QApplication.processEvents()
        time.sleep(random.uniform(0,0.7))
        self.ui.tbLog.setText(str(self.__check)+"% Completed")
        self.__check = self.__check + 1
        if(self.__check == 100):
          self.ui.leOutputFile.setText(str(self.__currentDirectory)+"/"+self.__layerT0+".tif")
        QtGui.QApplication.processEvents()
      
    
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
      self.threads = []
      # this will keep a reference to threads
      QtGui.QApplication.processEvents()
      for i in range(100):
            thread = MyThread(self)    # create a thread
            thread.trigger.connect(self.update_text)  # connect to it's signal
            QtGui.QApplication.processEvents()
            thread.setup(i)            # just setting up a parameter
            thread.start()             # start the thread
            self.threads.append(thread) # keep a reference
      thread1 = MyThread1(self)
      thread1.trigger1.connect(self.raster)
      thread1.setup(0)
      thread1.start()
      self.threads.append(thread1)
      #self.ui.leOutputFile.setText(str(self.__currentDirectory)+"/"+self.__layerT0+".tif")
      
  def raster(self, thread1_no):
          r_rasterize = R.globalenv['rasterise']
          r_rasterize(self.__shpfileT0,self.__layerT0,self.ui.sbGridsize.value())
  

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
    

  def getCurrentDirectory():
        return(self.__currentDirectory)
        

  @pyqtSignature("")
  def on_pbSelectFile_OutputFile_clicked(self):        
        shutil.move(self.__layerT0+".tif",str(self.__projectDirectory))
        self.ui.tbLog.setText(self.__layerT0 + ".tif saved to " + str(self.__projectDirectory))

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
    self.pbSelectDriver = QtGui.QPushButton()
    self.pbSelectDriver.setText("Select Driver")
    self.pbSelectDriver.setObjectName("pbSelectDriver"+str(self.ui.tableWidget.rowCount()-1))
    self.ui.tableWidget.setCellWidget(self.ui.tableWidget.rowCount()-1, 2, self.pbSelectDriver)
    self.makeCombo()    
    self.ui.tableWidget.setCellWidget(self.ui.tableWidget.rowCount()-1, 3, self.cbModel)

  def makeCombo(self):
    self.cbModel = QtGui.QComboBox()
    self.cbModel.setMaxCount(3)
    self.cbModel.setObjectName("cbModel")
    self.cbModel.addItem("iLoR")
    self.cbModel.addItem("iLiR")
    self.cbModel.addItem("iMLP")
    self.cbModel.setItemText(0, QtGui.QApplication.translate("", "Logistic R", None, QtGui.QApplication.UnicodeUTF8))
    self.cbModel.setItemText(1, QtGui.QApplication.translate("", "Linear R", None, QtGui.QApplication.UnicodeUTF8))
    self.cbModel.setItemText(2, QtGui.QApplication.translate("", "MLP", None, QtGui.QApplication.UnicodeUTF8))
       

  @pyqtSignature("")
  def on_rbLogR_clicked(self):
	i=0	
	while i<self.ui.tableWidget.rowCount():
		self.label = QtGui.QLabel("Logistic R")	
		self.ui.tableWidget.setCellWidget(i, 3,self.label)
		i=i+1
    
  @pyqtSignature("")
  def on_rbLinR_clicked(self):
	i=0	
	while i<self.ui.tableWidget.rowCount():
		self.label = QtGui.QLabel("Linear R")	
		self.ui.tableWidget.setCellWidget(i, 3,self.label)
		i=i+1

  @pyqtSignature("")
  def on_rbMLP_clicked(self):
	i=0	
	while i<self.ui.tableWidget.rowCount():
		self.label = QtGui.QLabel("MLP")	
		self.ui.tableWidget.setCellWidget(i, 3,self.label)
		i=i+1

  @pyqtSignature("")
  def on_rbIS_clicked(self):
	i=0	
	while i<self.ui.tableWidget.rowCount():	
		self.makeCombo()
		self.ui.tableWidget.setCellWidget(i, 3,self.cbModel)
		i=i+1

if __name__ == "__main__":
  app = QtGui.QApplication(sys.argv)
  myapp = MyForm()
  myapp.show()
  sys.exit(app.exec_())
