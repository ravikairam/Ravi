#Import the Libaries
import cv2
import numpy as np
import pytesseract
import os
import glob
import subprocess
import math
import utils
from matplotlib import pyplot as plt

#########################################
#   Tool Paths
#########################################

BaseDir = os.path.realpath("D:\\Practice\\Airbus - Drawing QC\\Drawing QC")
scriptDir = BaseDir + "\\Code" 

#Setup the Tesseract Path
pytesseract.pytesseract.tesseract_cmd = scriptDir + "\\Tesseract-OCR\\tesseract"

#Setup the PDF Converter Path
pdf_converter = scriptDir + "\\XPDF\\pdftopng.exe"

#Setup the temp storage
pages_path = scriptDir + "\\Temp"

#########################################
#   Functions
#########################################

# Convert the pdf to images:
def pdf_to_pages(pdf_folder):
    
	global pages_path
	global pdf_converter    
    
	print("Removing old Temp Pages...")
	exist_png = glob.glob(pages_path + "\\*.png")
	for filename in exist_png:
		os.remove(filename)
    
	print("Generating New Set Of Pages for " + pdf_folder + "\n")
	exist_pdf = glob.glob(pdf_folder + "\\*.pdf")
	os.chdir(pages_path)
    
	for pdf_file in exist_pdf:
		base=os.path.basename(pdf_file)
		filename = os.path.splitext(base)[0]        
		runScript = "\""+pdf_converter + "\" \"" + pdf_file + "\" " + filename        
		subprocess.call(runScript,shell=True)
        
	os.chdir(scriptDir)
	
# Get the Balloons
def get_balloons(page,debug=False):
	
	img_gray = cv2.cvtColor(page, cv2.COLOR_BGR2GRAY)
	circles = cv2.HoughCircles(img_gray, cv2.HOUGH_GRADIENT, 2, 15, param1=1300, param2=150, minRadius=40,maxRadius=50)
	
	output = page.copy()
	circle_list = []
	if circles is not None:
		circles = np.round(circles[0, :]).astype("int")
		for (x, y, r) in circles:
			if r>=13 and r<=16:	
				roi=img_gray[y-9:y+9,x-10:x+10]
				roi_big = cv2.resize(roi,None,fx=3, fy=3, interpolation = cv2.INTER_CUBIC)				
				text=pytesseract.image_to_string(roi_big,config='-psm 10')
				if utils.is_number(text):
					circle_list.append((x-10, y-9, x+10,y+9,text))
					#print(text + "-" + str(r))
					#cv2.rectangle(output, (x-10, y-9), (x+10, y+9), (0, 255, 0), 1)				
					if debug==True:
						cv2.putText(output,text, (x-20, y-20), cv2.FONT_HERSHEY_SIMPLEX, 1, (255, 0, 0), 1)
						cv2.circle(output, (x, y), r, (0, 255, 0), 4)
		
	return circle_list

# Get Notes
def get_notes(page):
	
	img_gray = cv2.cvtColor(img_rgb, cv2.COLOR_BGR2GRAY)
	
	template = cv2.imread('Code\\Templates\\note.png',0)
	w, h = template.shape[::-1]
	
	res = cv2.matchTemplate(img_gray,template,cv2.TM_CCOEFF_NORMED)
	threshold = 0.5
	loc = np.where( res >= threshold)
	notes_list=[]
	for pt in zip(*loc[::-1]):
		notes_list.append((pt[0],pt[1],pt[0]+w,pt[1]+h))
	
	notes_list = utils.non_max_suppression(np.array(notes_list),0.3)
    
	notes_list_final = []
	for x1,y1,x2,y2 in notes_list:
		roi=img_gray[y1+16:y2-15,x1+30:x2-30]
		roi_big = cv2.resize(roi,None,fx=3, fy=3, interpolation = cv2.INTER_CUBIC)				
		text=pytesseract.image_to_string(roi_big,config='-psm 10')		
		if utils.is_number(text):
			zone = get_zone(x1,y1)
			notes_list_final.append((x1,y1,x2,y2,text,zone))
	
	return notes_list_final

#Get the Zones based on XY Coordinates
def get_zone(x,y):
    
	#Get the Number Zone
	if x<265:
		nZone = 24
	else:
		print(24-math.floor((x-265)/295))
		nZone = 23-math.floor((x-265)/295)
		if nZone<1 :
			nZone = 1
            
    #Get the Alpha Zone
	aZone = ""
	aList = ['A','B','C','D','E','F','G','H','J','K','L','M','N','P','Q']
	if y<415:
		aZone = "R"
	else:		
		aZoneN = 14-math.floor((y-415)/295)
		if aZoneN < 0 :
			aZoneN = 0
		aZone  = aList[aZoneN]

	return aZone + str(nZone)

######################################
#   Code Starts Here             
######################################

#Generate the Pictures from the file
pdf_path = BaseDir + "\\Inputs"
#pdf_to_pages(pdf_path)
pdf_pages = glob.glob(pages_path + "\\*.png")

page = pdf_pages[1]
img_rgb = cv2.imread(page)
img_gray = cv2.cvtColor(img_rgb, cv2.COLOR_BGR2GRAY)

output = img_rgb.copy()
output = cv2.cvtColor(output, cv2.COLOR_BGR2RGB)
template = cv2.imread('Code\\Templates\\after.png',0)
w, h = template.shape[::-1]

res = cv2.matchTemplate(img_gray,template,cv2.TM_CCOEFF_NORMED)
threshold = 0.8
loc = np.where( res >= threshold)
for pt in zip(*loc[::-1]):
	cv2.rectangle(output,(pt[0],pt[1]),(pt[0]+w,pt[1]+h),(255,0,0),4)
	
plt.imshow(output)