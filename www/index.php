
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> Welcome to the spatiotemporal S4 class/method project.  We are getting setup right now.  Please join our project! </p>

<h2>Examples</h2>

   <h3 class="likesectionHead"><a 
 id="x1-2000"></a>Introduction</h3>
<!--l. 24--><p class="noindent" >Note: This vignette was directly copied from the sp package, then edited. Hopefully they will see this as
flattery...
<!--l. 27--><p class="indent" >   The   <span 
class="aett-10">spt </span>package provides S4 classes and methods for dealing with spatiotemporal data in S (R and
S-Plus<span class="footnote-mark"><a 
href="spt2.html#fn1x0"><sup class="textsuperscript">1</sup></a></span><a 
 id="x1-2001f1"></a> ).
The spatiotemporal data structures implemented currently include points, with support for lines, polygons and grids
coming in the next release, 0.01-02. We have chosen to use S4 classes and methods style (Chambers, 1998) to allow
validation of objects created. Although we mainly aim at using spatiotemporal data in the geographical
(two-dimensional) domain, the data structures that have a straightforward implementation in higher dimensions
(points, grids) do allow this.
<!--l. 37--><p class="indent" >   The motivation to write this package was born from my work at the EPA as a postdoc along with <a 
href="http://www.samsi.info/programs/2009spatialprogram.shtml" >the
SAMSI program on Spatiotemporal analysis</a> where I had the privilege to be a party crasher. At this
time, there are no packages with broad support for spatiotemporal data, however there are various
associated
<!--l. 44--><p class="indent" >   The package is available on R-forge. From the package home page, <a 
href="http://spt.r-forge.r-project.org" class="url" ><span 
class="aett-10">http://spt.r-forge.r-project.org</span></a>, a
graph gallery with R code, and the development source tree are available.
<!--l. 48--><p class="indent" >   This vignette describes the initial classes, methods and functions provided by <span 
class="aett-10">spt</span>. Instead of manipulating the
class slots (components) directly, we provide methods and functions to create the classes from elementary types such
as data.frames or vectors and to convert them back to any of these types. Also, coercion (type casting) from one
class to the other is provided sparsely, where relevant. If something isn&#8217;t provide, please request it from the
author(s).
                                                                                         
                                                                                         
<!--l. 56--><p class="indent" >   Package   <span 
class="aett-10">spt </span>is installed by
   <div class="fancyvrb" id="fancyvrb1">
<a 
 id="x1-2003r1"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;install.packages("spt",</span><span 
class="aesltt-10x-x-90">&#x00A0;repos</span><span 
class="aesltt-10x-x-90">&#x00A0;=</span><span 
class="aesltt-10x-x-90">&#x00A0;"http://R-forge.R-project.org")</span>
   </div>
<!--l. 65--><p class="indent" >   Currently, I put a lot of dependencies and they&#8217;re not all necessary now (especially the suggested
packages).
<!--l. 68--><p class="indent" >   Package <span 
class="aett-10">spt </span>is loaded by
   <div class="fancyvrb" id="fancyvrb2">
<a 
 id="x1-2005r1"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;library(spt)</span>
   </div>
   <h3 class="likesectionHead"><a 
 id="x1-3000"></a>Spatiotemporal Data Classes</h3>
<!--l. 81--><p class="noindent" >Spatiotemporal data is a complicated affair. There are many, many types of spatiotemporal data and many, many
uses of it. I do not presuppose to help all users with all applications. The current efforts only pertain to my current
work with air pollution data, monitoring sites located at lat/long locations <span 
class="cmr-10">(</span><span 
class="cmmi-10">x,y</span><span 
class="cmr-10">)</span>, producing an observation such as
the measurement of ozone in ppm at regular time intervals (eg hours, days, etc), usually which have some relation to
the regulatory standard (I have the outline of functions to calculate the primary and secondary ozone standard, for
example). These stations eventually produce flat (eg text) files which are 4 columns (lat, long, time, obs) and <span 
class="cmmi-10">n</span><sub><span 
class="cmmi-7">obs</span></sub>
rows.
<!--l. 94--><p class="noindent" >
   <h4 class="likesubsectionHead"><a 
 id="x1-4000"></a>Spatiotemporal Classes</h4>
<!--l. 95--><p class="noindent" >The basic structure of spatiotemporal classes consists of 3 core classes and 1 optional class. The core classes are for
storing the temporal part of the data, the spatial part of the data, and the observations. These are stored
somewhat independently, with a unique identifier for the time and spatial location providing the map
between the classes, in the spirit of a relational database. This has several advantages. First, the bulky
temporal and spatial parts are reduced to their unique elements, reducing storage space and speeding
up computation by performing operations on the unique elements, then duplicating those results as
needed. Second, it allows the data to be stored in different places. This is not currently implemented,
but it would allow the spatial parts to be stored using, say, the <span 
class="aett-10">SQLiteMap </span>package for the spatial
part.
<!--l. 109--><p class="indent" >   The current class is <span 
class="aett-10">SpatialTemporalDataFrame</span>. However, only point data is currently (v 0.01-01)
supported.
<!--l. 112--><p class="indent" >   Currently, this class contains objects of the other classes. From a big picture view, I don&#8217;t know if it&#8217;s better to
use <span 
class="aett-10">setClassUnion </span>instead.
<!--l. 116--><p class="noindent" >
   <h4 class="likesubsectionHead"><a 
 id="x1-5000"></a>Temporal Classes</h4>
<!--l. 117--><p class="noindent" >Currently, the temporal class is called <span 
class="aett-10">stTemporal</span>. It consists of a pairlist (but it&#8217;s currently 2 vectors, the integer
unique temporal IDs, and the timeDate time stamps.
<!--l. 121--><p class="indent" >   The format is very important to ensure that the timedates are read correctly. Please visit the help page for
<span 
class="aett-10">strptime </span>for the complete list.
<!--l. 125--><p class="indent" >   Example: create, get,
<!--l. 129--><p class="indent" >
                                                                                         
                                                                                         
   <div class="fancyvrb" id="fancyvrb3">
<a 
 id="x1-5002r1"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;tmp</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003C;-</span><span 
class="aesltt-10x-x-90">&#x00A0;stTemporal(timeVals</span><span 
class="aesltt-10x-x-90">&#x00A0;=</span><span 
class="aesltt-10x-x-90">&#x00A0;c("2008-09-29",</span><span 
class="aesltt-10x-x-90">&#x00A0;"2009-01-14",</span><span 
class="aesltt-10x-x-90">&#x00A0;"2012-12-12"),</span>
<br class="fancyvrb" /><a 
 id="x1-5004r2"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;+</span><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;format</span><span 
class="aesltt-10x-x-90">&#x00A0;=</span><span 
class="aesltt-10x-x-90">&#x00A0;"%Y-%m-%d")</span>
<br class="fancyvrb" /><a 
 id="x1-5006r3"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;tmp</span>
<br class="fancyvrb" /><a 
 id="x1-5008r4"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;getTid(tmp)</span>
   </div>
<!--l. 135--><p class="noindent" >
   <div class="fancyvrb" id="fancyvrb4">
<a 
 id="x1-5010r1"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[1]</span><span 
class="aett9-">&#x00A0;1</span><span 
class="aett9-">&#x00A0;2</span><span 
class="aett9-">&#x00A0;3</span>
   </div>
<!--l. 138--><p class="noindent" >
   <div class="fancyvrb" id="fancyvrb5">
<a 
 id="x1-5012r1"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;getTimedatestamps(tmp)</span>
   </div>
<!--l. 141--><p class="noindent" >
   <div class="fancyvrb" id="fancyvrb6">
<a 
 id="x1-5014r1"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[1]</span><span 
class="aett9-">&#x00A0;"2008-09-29"</span><span 
class="aett9-">&#x00A0;"2009-01-14"</span><span 
class="aett9-">&#x00A0;"2012-12-12"</span>
   </div>
<!--l. 147--><p class="noindent" >
   <h4 class="likesubsectionHead"><a 
 id="x1-6000"></a>Spatial Classes</h4>
<!--l. 148--><p class="noindent" >Currently, the spatial class is called <span 
class="aett-10">stSpatial</span>. It consists of 2 objects, the integer unique spatial IDs, and a
<span 
class="aett-10">Spatial </span>object, currently only <span 
class="aett-10">SpatialPoints </span>is supported.
<!--l. 153--><p class="indent" >   Please visit the vignett for the <span 
class="aett-10">sp </span>package for the complete list of items to be supported in version
1. Note that none of the data containing classes will be supported here since we have our own data
classes.
<!--l. 158--><p class="indent" >   Currently I inherit the spatial class, and I&#8217;m not sure what the implications are for that (there&#8217;s a big difference
whether you contain an object of a class or whether you extend a class).
<!--l. 162--><p class="indent" >   Example: create, get,
<!--l. 167--><p class="indent" >
   <div class="fancyvrb" id="fancyvrb7">
<a 
 id="x1-6002r1"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;n</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003C;-</span><span 
class="aesltt-10x-x-90">&#x00A0;10</span>
<br class="fancyvrb" /><a 
 id="x1-6004r2"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;d</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003C;-</span><span 
class="aesltt-10x-x-90">&#x00A0;2</span>
<br class="fancyvrb" /><a 
 id="x1-6006r3"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;crd</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003C;-</span><span 
class="aesltt-10x-x-90">&#x00A0;matrix(runif(n</span><span 
class="aesltt-10x-x-90">&#x00A0;*</span><span 
class="aesltt-10x-x-90">&#x00A0;d),</span><span 
class="aesltt-10x-x-90">&#x00A0;n,</span><span 
class="aesltt-10x-x-90">&#x00A0;d)</span>
<br class="fancyvrb" /><a 
 id="x1-6008r4"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;bbox</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003C;-</span><span 
class="aesltt-10x-x-90">&#x00A0;cbind(apply(crd,</span><span 
class="aesltt-10x-x-90">&#x00A0;2,</span><span 
class="aesltt-10x-x-90">&#x00A0;min),</span><span 
class="aesltt-10x-x-90">&#x00A0;apply(crd,</span><span 
class="aesltt-10x-x-90">&#x00A0;2,</span><span 
class="aesltt-10x-x-90">&#x00A0;max))</span>
<br class="fancyvrb" /><a 
 id="x1-6010r5"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;colnames(bbox)</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003C;-</span><span 
class="aesltt-10x-x-90">&#x00A0;c("min",</span><span 
class="aesltt-10x-x-90">&#x00A0;"max")</span>
<br class="fancyvrb" /><a 
 id="x1-6012r6"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;tmp</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003C;-</span><span 
class="aesltt-10x-x-90">&#x00A0;new("stSpatialPoints",</span><span 
class="aesltt-10x-x-90">&#x00A0;s.id</span><span 
class="aesltt-10x-x-90">&#x00A0;=</span><span 
class="aesltt-10x-x-90">&#x00A0;as.integer(1:n),</span><span 
class="aesltt-10x-x-90">&#x00A0;coords</span><span 
class="aesltt-10x-x-90">&#x00A0;=</span><span 
class="aesltt-10x-x-90">&#x00A0;crd,</span>
<br class="fancyvrb" /><a 
 id="x1-6014r7"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;+</span><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;bbox</span><span 
class="aesltt-10x-x-90">&#x00A0;=</span><span 
class="aesltt-10x-x-90">&#x00A0;bbox,</span><span 
class="aesltt-10x-x-90">&#x00A0;proj4string</span><span 
class="aesltt-10x-x-90">&#x00A0;=</span><span 
class="aesltt-10x-x-90">&#x00A0;CRS(as.character(NA)))</span>
<br class="fancyvrb" /><a 
 id="x1-6016r8"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;tmp</span>
<br class="fancyvrb" /><a 
 id="x1-6018r9"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;getSid(tmp)</span>
   </div>
<!--l. 178--><p class="noindent" >
   <div class="fancyvrb" id="fancyvrb8">
<a 
 id="x1-6020r1"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[1]</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;1</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;2</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;3</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;4</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;5</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;6</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;7</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;8</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;9</span><span 
class="aett9-">&#x00A0;10</span>
   </div>
<!--l. 181--><p class="noindent" >
                                                                                         
                                                                                         
   <div class="fancyvrb" id="fancyvrb9">
<a 
 id="x1-6022r1"></a><span 
class="aesltt-10x-x-90">&#x00A0;</span><span 
class="aesltt-10x-x-90">&#x00A0;&#x003E;</span><span 
class="aesltt-10x-x-90">&#x00A0;getSpatialPoints(tmp)</span>
   </div>
<!--l. 184--><p class="noindent" >
   <div class="fancyvrb" id="fancyvrb10">
<a 
 id="x1-6024r1"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;SpatialPoints:</span>
<br class="fancyvrb" /><a 
 id="x1-6026r2"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[,1]</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[,2]</span>
<br class="fancyvrb" /><a 
 id="x1-6028r3"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[1,]</span><span 
class="aett9-">&#x00A0;0.6739453</span><span 
class="aett9-">&#x00A0;0.3021973</span>
<br class="fancyvrb" /><a 
 id="x1-6030r4"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[2,]</span><span 
class="aett9-">&#x00A0;0.9619853</span><span 
class="aett9-">&#x00A0;0.6170330</span>
<br class="fancyvrb" /><a 
 id="x1-6032r5"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[3,]</span><span 
class="aett9-">&#x00A0;0.9228386</span><span 
class="aett9-">&#x00A0;0.9050982</span>
<br class="fancyvrb" /><a 
 id="x1-6034r6"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[4,]</span><span 
class="aett9-">&#x00A0;0.7747456</span><span 
class="aett9-">&#x00A0;0.8476483</span>
<br class="fancyvrb" /><a 
 id="x1-6036r7"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[5,]</span><span 
class="aett9-">&#x00A0;0.7248062</span><span 
class="aett9-">&#x00A0;0.4579902</span>
<br class="fancyvrb" /><a 
 id="x1-6038r8"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[6,]</span><span 
class="aett9-">&#x00A0;0.7413480</span><span 
class="aett9-">&#x00A0;0.3863993</span>
<br class="fancyvrb" /><a 
 id="x1-6040r9"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[7,]</span><span 
class="aett9-">&#x00A0;0.6302933</span><span 
class="aett9-">&#x00A0;0.6443511</span>
<br class="fancyvrb" /><a 
 id="x1-6042r10"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[8,]</span><span 
class="aett9-">&#x00A0;0.1903071</span><span 
class="aett9-">&#x00A0;0.7170724</span>
<br class="fancyvrb" /><a 
 id="x1-6044r11"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[9,]</span><span 
class="aett9-">&#x00A0;0.6977443</span><span 
class="aett9-">&#x00A0;0.1980982</span>
<br class="fancyvrb" /><a 
 id="x1-6046r12"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;[10,]</span><span 
class="aett9-">&#x00A0;0.3703263</span><span 
class="aett9-">&#x00A0;0.2772779</span>
<br class="fancyvrb" /><a 
 id="x1-6048r13"></a><span 
class="aett9-">&#x00A0;</span><span 
class="aett9-">&#x00A0;Coordinate</span><span 
class="aett9-">&#x00A0;Reference</span><span 
class="aett9-">&#x00A0;System</span><span 
class="aett9-">&#x00A0;(CRS)</span><span 
class="aett9-">&#x00A0;arguments:</span><span 
class="aett9-">&#x00A0;NA</span>
   </div>

<h2>Notes</h2>
<ul>
<li> Always enter spatial coordinates in the format <TT>(x,y)</TT> or <TT>(long, lat)</TT></li>
<li> In grids, the vector form is column major order</li>
<li> For distances, you have several choices: euclidean or great circle,  for great circle you have to choose units (miles, kilometers)</li>
<!-- <li></li> -->
</ul>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
