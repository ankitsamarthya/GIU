import sys, os, random, time, re
import shutil
import os.path
from os.path import basename, splitext
from array import array

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
  k=0
  __filename = []
  customlist = []
  res = ""
  values = []
  __T1File = ""
  __T0File = ""
  DriverType = ""
  Drivername = []
  DictList = {}
  DictLen = 0
 
  

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
      
  def getToolTip(self, str1):
    layer = []
    layerlist = []
    for i in range(0,len(self.DictList),1):
      if(i<(len(self.DictList)-1)):
        layer.append(str1[str1.index(str(self.DictList.keys()[i]))+len(str(self.DictList.keys()[i])):str1.index(str(self.DictList.keys()[i+1]))])  
      else:
        layer.append(str1[str1.index(str(self.DictList.keys()[i]))+len(str(self.DictList.keys()[i])):str1.index("---")])
    
    for j in range(0,len(layer),1):
      layerlist.append(layer[j].split())
  
    return layerlist
  
  def checkStar(self, list1):
    if(list1[-1]=="***" or list1[-1]=="**" or list1[-1]=="*"):
        _11s=list1[-1]
    else:
        _11s=""
    return _11s
  
  
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
        fileType = splitext(fileName)[1]
        if(fileType == ".tif" or fileType == ".tiff"):
          self.__T0File=str(file)
        else:
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
          if(self.ui.leYear_T0.text()!=""):
            self.__layerT0=str(self.ui.leYear_T0.text())
          r_rasterize(self.__shpfileT0,self.__layerT0,self.ui.sbGridsize.value())
          self.__T0File = str(self.__currentDirectory)+"/"+self.__layerT0+".tif"
          self.ui.leT0File.setText(self.__T0File)
  

  @pyqtSignature("")
  def on_pbSelectFile_T1_clicked(self):
        file = QtGui.QFileDialog.getOpenFileName(self, "Open File",
                            self.__currentDirectory,"Raster (*.tiff *.tif *.shp)");
        (dirName, fileName) = os.path.split(str(file))
        self.__currentDirectory=dirName
        self.__layerT1=splitext(fileName)[0]
        fileType = splitext(fileName)[1]
        if(fileType == ".tif" or fileType == ".tiff"):
          self.__T1File=str(file)
        else:
          self.__shpfileT1 = str(file)
        self.ui.leT1File.setText(str(file))
  
  @pyqtSignature("")
  def on_pbConvert_T1_clicked(self):
      r_rasterize = R.globalenv['rasterise']
      if(self.ui.leYear_T1.text()!=""):
            self.__layerT1=str(self.ui.leYear_T1.text())
      r_rasterize(self.__shpfileT1,self.__layerT1,self.ui.sbGridsize.value())
      self.__T1File = str(self.__currentDirectory)+"/"+self.__layerT1+".tif"
      self.ui.leT1File.setText(self.__T1File)

  @pyqtSignature("")

  def on_pbSelectFile_Mask_clicked(self):
        file = QtGui.QFileDialog.getOpenFileName(self, "Open File",
                            self.__currentDirectory,"Raster (*.tiff *.tif )");
        (dirName, fileName) = os.path.split(str(file))
        self.__currentDirectory=dirName
        self.ui.leMaskFile.setText(str(file))
    

  def getCurrentDirectory():
        return(self.__currentDirectory)
        

  #@pyqtSignature("")
  #def on_pbSelectFile_OutputFile_clicked(self):        
  #      shutil.move(self.__layerT0+".tif",str(self.__projectDirectory))
  #      self.ui.tbLog.setText(self.__layerT0 + ".tif saved to " + str(self.__projectDirectory))

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



  @QtCore.pyqtSlot() # prevents executing following function twice
  def cell_was_clicked(self):
    sending_button = self.sender()
    row = int(sending_button.objectName())    
    file = QtGui.QFileDialog.getOpenFileName(self, "Open File",self.__currentDirectory,"Raster (*.img)");
        #(dirName, fileName) = os.path.split(str(file))
        #self.__currentDirectory=dirName
        #self.__layerT1=splitext(fileName)[0]
        #self.__shpfileT1 = str(file)
    self.__filename.append(str(file))
    self.disp = QtGui.QLabel(str(file))
    self.ui.tableWidget.setCellWidget(row, 1,self.disp)
    print(self.__filename[self.k])
    self.k = self.k + 1   
        #self.ui.leT1File.setText(str(file))
        
    

  @pyqtSignature("")
  def on_pbAddDriver_clicked(self):
    self.ui.tableWidget.setRowCount(self.ui.tableWidget.rowCount() + 1)
    self.pbSelectDriver = QtGui.QPushButton()
    self.pbSelectDriver.setText("Select Driver")
    self.pbSelectDriver.setObjectName(str(self.ui.tableWidget.rowCount()-1))
    self.Drivername.append("Driver"+str(self.ui.tableWidget.rowCount()))
    #item1 = self.ui.tableWidget.item(self.ui.tableWidget.rowCount()-1,0)
    #item1.setText(self.Drivername[self.ui.tableWidget.rowCount()-1])
    #self.ui.tableWidget.item(self.ui.tableWidget.rowCount()-1,0).text="abc"
    nameLabel = QtGui.QLineEdit(self.Drivername[self.ui.tableWidget.rowCount()-1])
    self.pbSelectDriver.clicked.connect(self.cell_was_clicked)
    self.ui.tableWidget.setCellWidget(self.ui.tableWidget.rowCount()-1, 0, nameLabel)
    self.ui.tableWidget.setCellWidget(self.ui.tableWidget.rowCount()-1, 2, self.pbSelectDriver)
    self.makeCombo()    
    self.ui.tableWidget.setCellWidget(self.ui.tableWidget.rowCount()-1, 3, self.cbModel)
    
  def on_pbExecute_clicked(self):
    R.r('''  source('Rasterise_dev_61.R')  ''')
    for i in range(0,self.ui.tableWidget.rowCount()):
      self.Drivername[i] = self.ui.tableWidget.cellWidget(i,0).text()
      self.DictList[str(self.Drivername[i])] = self.__filename[i]
    self.DictLen = len(self.DictList)
      
    
    drvs = R.ListVector(self.DictList)
    genSummary = R.r['genrateStatisticalSummary']
    self.res = genSummary(str(self.DriverType),self.__T0File,self.__T1File,drvs,int(self.ui.leNAValue.text()))
    self.ui.teSummary.setPlainText(str(self.res))

  def initialize(self, row, col):
    
    for i in range(0, row, 1):
      for j in range(2, col, 1):
        item = QtGui.QTableWidgetItem()
        self.ui.twDriverSelection.setItem(i, j, item)
    
    for i in range(0, row, 1):
      for j in range(1, col-1, 1):
        item = QtGui.QTableWidgetItem()
        self.ui.twCreateSuitability.setItem(i, j, item)
             
    for i in range(0, row, 1):
      for j in range(2, col, 1):
        item = self.ui.twDriverSelection.item(i,j)
        item.setFlags(QtCore.Qt.ItemIsDragEnabled|QtCore.Qt.ItemIsUserCheckable|QtCore.Qt.ItemIsEnabled)
        item.setCheckState(QtCore.Qt.Unchecked)
        
    for i in range(0, row, 1):
      for j in range(1, col-1, 1):
        item = self.ui.twCreateSuitability.item(i,j)
        item.setFlags(QtCore.Qt.ItemIsDragEnabled|QtCore.Qt.ItemIsUserCheckable|QtCore.Qt.ItemIsEnabled)
        item.setCheckState(QtCore.Qt.Unchecked)
        
    stringlist1 = QtCore.QStringList()
    stringlist1.append('Class')
    stringlist1.append('ClassNumber')
    for i in range(0,len(self.DictList),1):
      stringlist1.append(str(self.DictList.keys()[i]))
   
    self.ui.twDriverSelection.setHorizontalHeaderLabels(stringlist1)
    
    stringlist2 = QtCore.QStringList()
    stringlist2.append('Class')
    for i in range(0,len(self.DictList),1):
      stringlist2.append(str(self.DictList.keys()[i]))
   
    self.ui.twCreateSuitability.setHorizontalHeaderLabels(stringlist2)
      
    

  
    
  def on_pbNextCreate_clicked(self):
    
    self.ui.twDriverSelection.setColumnCount(self.ui.twDriverSelection.columnCount()+(self.DictLen-(self.ui.twDriverSelection.columnCount()-2)))
    self.ui.twCreateSuitability.setRowCount(self.ui.twDriverSelection.rowCount())
    self.ui.twCreateSuitability.setColumnCount((self.ui.twDriverSelection.columnCount()+(self.DictLen-(self.ui.twDriverSelection.columnCount()-2)))-1)
    
    self.initialize(self.ui.twDriverSelection.rowCount(),self.ui.twDriverSelection.columnCount())
    R.r('''  source('Rasterise_dev_61.R')  ''')
    getClassNum = R.r['getClassNumber']
    res = getClassNum(self.__T0File)
    customlist = re.split('\[\[\d{1,}\]\]',str(self.res))
    final = []
    for i in range(1,self.ui.twDriverSelection.rowCount()+1,1):    
      final.append(self.getToolTip(customlist[i]))
    
    for i in range(0,self.ui.twDriverSelection.rowCount(),1):
      item1 = self.ui.twDriverSelection.item(i,1)
      item1.setText(str(res[i]))
    
    
    for j in range(0,self.ui.twDriverSelection.rowCount(),1):
      for k in range(2,self.ui.twDriverSelection.columnCount(),1):
        item1 = self.ui.twDriverSelection.item(j,k)
        item1.setText(self.__filename[k-2])
        self.toolTip(final[j],j,k)
    
    self.ui.tabWidget.setCurrentIndex(2)
    self.ui.progressBar.setProperty("value",30)
    
  def makeCombo(self):
    self.cbModel = QtGui.QComboBox()
    self.cbModel.setMaxCount(3)
    self.cbModel.setObjectName("cbModel"+str(self.ui.tableWidget.rowCount()-1))
    self.cbModel.addItem("iLoR")
    self.cbModel.addItem("iLiR")
    self.cbModel.addItem("iMLP")
    self.cbModel.setItemText(0, QtGui.QApplication.translate("", "Logistic R", None, QtGui.QApplication.UnicodeUTF8))
    self.cbModel.setItemText(1, QtGui.QApplication.translate("", "Linear R", None, QtGui.QApplication.UnicodeUTF8))
    self.cbModel.setItemText(2, QtGui.QApplication.translate("", "MLP", None, QtGui.QApplication.UnicodeUTF8))
  
  def boxCheck(self):
    l=0
    for i in range(0,self.ui.twDriverSelection.rowCount(),1):
      for j in range(2,self.ui.twDriverSelection.columnCount(),1):
        item1 = self.ui.twDriverSelection.item(i,j)
        if(item1.checkState()==0):
          item = self.ui.twCreateSuitability.item(i,j-1)
          item.setCheckState(QtCore.Qt.Unchecked)
          item.setText(self.values[l])
          l=l+1
        else:
          item = self.ui.twCreateSuitability.item(i,j-1)
          item.setCheckState(QtCore.Qt.Checked)
          item.setText(self.values[l])
          l=l+1
    
  def on_pbNextSuitable_clicked(self):
    self.boxCheck()
    self.ui.tabWidget.setCurrentIndex(3)
    self.ui.progressBar.setProperty("value",40)
  
       
  def toolTip(self, final, m, n):
    self.item = self.ui.twDriverSelection.item(m, n)
    self.values.append(final[n-2][0])
    toolstr = str(final[n-2][0])+" "+str(self.checkStar(final[n-2]))
    self.item.setToolTip(QtGui.QApplication.translate("LULCModel", toolstr, None, QtGui.QApplication.UnicodeUTF8))
    return
  
  def on_pbExecuteAllocation_clicked(self):
    R.r('''  source('Rasterise_dev_61.R')  ''')
    drvs = R.StrVector(self.__filename)
    classlist1 = []
    for i in range(0,self.ui.twDriverSelection.rowCount(),1):
      item1 = self.ui.twDriverSelection.item(i,0) 
      classlist1.append(str(item1.text()))
    
    classList = R.StrVector(classlist1)
    genMap = R.r['genratePredictedMap']
    genMap('logistic',self.__T0File,self.__T1File,classList,drvs,drvs,int(self.ui.leNAValue.text()),str(self.ui.leOutputFile.text()))
    #self.ui.teSummary.setPlainText(str(self.res))
  
  def on_pbNextDemandAlloc_clicked(self):
    self.ui.twDemandAlloc.setRowCount(self.ui.twDriverSelection.rowCount())
    self.ui.tabWidget.setCurrentIndex(4)
    self.ui.progressBar.setProperty("value",50)
  
  @pyqtSignature("")
  def on_rbLogR_clicked(self):
	i=0
        self.DriverType = "logistic"
	while i<self.ui.tableWidget.rowCount():
		self.label = QtGui.QLabel("Logistic R")	
		self.ui.tableWidget.setCellWidget(i, 3,self.label)
		i=i+1
    
  @pyqtSignature("")
  def on_rbLinR_clicked(self):
	i=0
        self.DriverType = "linear"
	while i<self.ui.tableWidget.rowCount():
		self.label = QtGui.QLabel("Linear R")	
		self.ui.tableWidget.setCellWidget(i, 3,self.label)
		i=i+1

  @pyqtSignature("")
  def on_rbMLP_clicked(self):
	i=0
        self.DriverType = "mlp"
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
