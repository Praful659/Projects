libname clasdata "H:\Assignment_3";

/* 1) a) Reading Data */
data clasdata.lap;
  SET clasdata.laptop;
run;
proc print data = clasdata.lap;
run;

/* 1) a) i) & ii) Cleaning Data */
DATA clasdata.lap; 
   SET clasdata.lap; 
   IF NOT (brand IN ('ACER', 'APPLE', 'ASUS', 'DELL', 'DYNABOOK', 'HP', 'LENOVO')) THEN brand = 'OTHER';
   IF operating_system NE 'WINDOWS' THEN operating_system = 'OTHERS';
RUN;
proc print data= clasdata.lap;
run;

/* 1) b) Understanding data types */
PROC CONTENTS DATA = clasdata.lap;
run;

/* 1) c) Variable Frequencies */
PROC FREQ DATA=clasdata.lap; 
   TABLES brand operating_system refurbished; 
RUN;

/* 1) d) i) ii) iii) Creating categorical variables */
DATA clasdata.lap; 
   SET clasdata.lap; 
if brand='APPLE' then Apple=1; else Apple=0;
if brand='DELL' then Dell=1; else Dell=0;
if brand='ASUS' then Asus=1; else Asus=0;
if brand='ACER' then Acer=1; else Acer=0;
if brand='DYNABOOK' then DB=1; else DB=0;
if brand='HP' then HP=1; else HP=0;
if brand='LENOVO' then Lenovo=1; else Lenovo=0;
if operating_system='WINDOWS' then is_windows=1; else is_windows=0;
if refurbished='YES' then is_refurbished=1; else is_refurbished=0;
ram_gb = ram_nth**2;
storage_gb = storage_nth**2;
proc print data= clasdata.lap;
run;


/* 1) e) Running Reg model */
proc reg data=clasdata.lap;
   model Price = cpu_benchmark gpu_class screen_size ppi warranty apple dell asus db hp lenovo is_windows is_refurbished ram_gb storage_gb;
run;


/* /////////1) e) i) ii) iii) Answered in report */

/* 1) f) taking log transformation of required variables*/
data clasdata.laplog;
set clasdata.lap;
lprice=log(price);
lscreen=log(screen_size);
lwarranty = log(warranty);

proc print data=clasdata.laplog;
run;

/* 1) g) Running log - log model*/
proc reg data= clasdata.laplog;
model lprice= lscreen lwarranty cpu_benchmark gpu_class ppi apple dell asus db hp lenovo is_windows is_refurbished ram_gb storage_gb;
run;

/* /////////1) g) Answered in the report*/
/* /////////1) h) Answered in the report*/

/* 2) a) Running proc glm*/
proc glm data= clasdata.laplog;
class brand operating_system refurbished;
model lprice=lscreen lwarranty cpu_benchmark gpu_class ppi operating_system brand refurbished ram_gb storage_gb /solution;
run;


/* 2) b) Running proc glm by changing ref*/
proc glm data= clasdata.laplog;
class brand (ref = 'OTHER') operating_system (ref = 'OTHERS') refurbished (ref = 'NO');
model lprice=lscreen lwarranty cpu_benchmark gpu_class ppi operating_system brand refurbished ram_gb storage_gb /solution;
run;
































