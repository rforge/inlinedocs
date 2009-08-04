
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

<!-- R-Forge Logo
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>
 -->

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

<h1>Simple and sustainable R packaging using inlinedocs</h1>

<p> You wrote a bunch of R code, now you want to share it with
everyone, so you are making a package. Most likely you have a bunch of
functions in a file, and now you have to write all the documentation
for them. Now how to go about doing that?
  </p>

<ul>

<li><b>Option 1: do it yourself.</b> Fire up package.skeleton() and
then edit the Rd files by hand, following the directions in <a
href="http://cran.r-project.org/doc/manuals/R-exts.pdf">Writing R
Extensions (135 pages)</a> --- good luck. <b>This sucks</b> because it
takes a long time to edit all these files by hand, and if you change
your code you'll end up with docs which do not agree. Unless you
constantly update all those Rd files. (will you really?)</li> 

<li><b>Option 2: <a href="http://roxygen.org/">Roxygen</a>.</b> You
like the idea of combining source code with documentation, because
then it's easier to keep them in sync with each other. But <b>Roxygen
sucks</b> because it makes you write huge bloated headers and repeat
yourself using its weird syntax (but to its credit, the call graphs
are pretty cool).</li>

<li><b>Option 3: use the inlinedocs package.</b> No syntax to
learn, never have to repeat yourself, just comment your code in
natural places, and you get Rd files that pass R CMD check so you can
publish that package and move on with your life. And when you make
changes to that function, the docs are right next to the code so odds
are you'll update the docs if they need to be.</li>

</ul>

<img src="inlinedocs-demo.png" />

<p>Then put your code in packagename/R/code.file.R, write a
packagename/DESCRIPTION, start R, and do:</p>

<pre>
> install.packages("inlinedocs",repos="http://r-forge.r-project.org")
> setwd("/path/to/your/packagename/R")
> library(inlinedocs)
> package.skeleton.dx()
</pre>

<p>That's it, you will get Rd files in packagename/man that are guaranteed to pass R CMD check! Some notes/tips:</p>

<ul>

<li>Use an editor where you can comment-fill easily, like <a href="http://www.gnu.org/software/emacs">Emacs</a> with <a href="http://ess.r-project.org">ESS</a>. Then you just type "###" and go on as long as you want with your comment. When you're done and you have a really long comment, do a M-q to automatically break lines and add ### prefixes.<li>

<li>Some examples:

<ul>

<li>inlinedocs:
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/inlinedocs/DESCRIPTION?rev=2&root=inlinedocs&view=markup">DESCRIPTION</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/inlinedocs/R/package.skeleton.dx.R?rev=12&root=inlinedocs&view=markup">code</a>
</li>

<li>sublogo:
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/DESCRIPTION?rev=3&root=sublogo&view=markup">DESCRIPTION</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/R/sublogo.dendrogram.R?rev=17&root=sublogo&view=markup">code</a>
</li>

<li>Do you use inlinedocs? If so, 
<a href="http://r-forge.r-project.org/sendmessage.php?touser=1571">
send me an email</a>
and I'll add your project to this list!
</li>

</ul>

</li>

<li>You can add a description of a data item declared in your code by
adding a ### comment on the line before the variable is first
declared.</li>

<li>Function names are used for the title section of the Rd file. Dots
in the function name are translated into spaces in the title.</li>

<li>The DESCRIPTION file is used by inlinedocs for the Maintainer
(this becomes the author secion of the Rd files) and Package (this is
used as the name argument to package.skeleton). However R CMD check
also makes you include: Version License Description Title Author. So
if you're really lazy (like me), and you haven't written one yet,
inlinedocs will create an empty DESCRIPTION file for you.

</li>

<li>You still have to manually document your datasets (the files in
packagename/data), but these don't change often, so that's no problem
to do by hand. To get started just use the prompt() function.</li>

<li>To add examples for FUN, add a file FUN.R to your
packagename/tests directory. This is more sustainable than putting
test code in R comments, since debugging/changing commented code is a
pain.</li>

<li>Yes I realize that the code style is different from how you
usually code (it's not how I usually code either). The idea here is
"convention over configuration" --- if you code this way, then you
don't have to worry about specifying all the crap that makes using
Roxygen a pain in the ass.</li>

</ul>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
