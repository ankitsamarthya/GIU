import rpy2.robjects as robjects

robjects.r('''
       source('Rasterise_dev_61.R')

''')

driverlist = []

driverlist.append('Slope.img')
driverlist.append('elevation.img')
driverlist.append('Dist_urban.img')
driverlist.append('dist_stream.img')
drvs = robjects.StrVector(driverlist)

genSummary = robjects.r['genrateStatisticalSummary']
res = genSummary('logistic','1985.tif','1995.tif',drvs,128)
print(res)
#print(robjects.r('summary'))

#getClassNum = robjects.r['getClassNumber']
#res = getClassNum('1985.tif')
#print(res)
