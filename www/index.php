
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

<h1>Simple and sustainable R packaging using inlinedocs</h1>

<p> You wrote a bunch of R code, now you want to share it with
everyone, so you are making a package. Most likely you have a bunch of
functions in a file, and now you have to write all the documentation
for them. Now how to go about doing that?
  </p>

<ul>

<li><b>Option 1: do it yourself.</b> Fire up package.skeleton() and
then edit the files by hand, following the directions in <a
href="http://cran.r-project.org/doc/manuals/R-exts.pdf">Writing R
Extensions (135 pages)</a> --- good luck. <b>This sucks</b> because it
takes a long time to edit all these files by hand, and if you change
your code you'll end up with docs which do not agree. Unless you
constantly update all those Rd files. (will you really?)</li> 

<li><b>Option 2: <a href="http://roxygen.org/">Roxygen</a>.</b> You like the idea of combining source code with documentation, because then its easier to keep them in sync with each other. But <b>Roxygen sucks</b> because it makes you write huge bloated headers and repeat yourself in a weird syntax that you need to learn.</li>

<li><b>Option 3: use the inlinedocs package.</b> No syntax to
learn, never have to repeat yourself, just comment your code in
natural places, and you get Rd files that pass R CMD check so you can
publish that package and move on with your life. And when you make
changes to that function, the docs are right next to the code so odds
are you'll update the docs if they need to be.</li>

<img src="inlinedocs-demo.png" />

<p>Then put your code in packagename/R/code.file.R, write a
packagename/DESCRIPTION, start R, and do:</p>

<pre>
> install.packages("inlinedocs",repos="http://r-forge.r-project.org")
> setwd("/path/to/your/packagename")
> library(inlinedocs)
> package.skeleton.dx()
</pre>

<p>That's it, you will get Rd files in packagename/man that are guaranteed to pass R CMD check! Some notes/tips:</p>

<ul>

<li>Use an editor where you can comment-fill easily, like <a href="http://www.gnu.org/software/emacs">Emacs</a> with <a href="http://ess.r-project.org">ESS</a>. Then you just type "###" and go on as long as you want with your comment. When you're done and you have a really long comment, do a M-q to automatically break lines and add ### prefixes.<li>

<li>None of these comments are necessary.</li>

<li>You can add a description of a data item declared in your code by
adding a ### comment on the line before the variable is first
declared.</li>

<li>You still have to document your datasets manually, but these don't change often, so thats no problem to do by hand. To get started just use the prompt() function.</li>

<li>Yes I realize that the format of the code is different from how
you usually code. The idea here is "convention over configuration" ---
learn to code this way, and then you don't have to worry about specifying all
the crap that makes using Roxygen a pain in the ass.</li>

</ul>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
