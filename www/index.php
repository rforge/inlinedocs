
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
Extensions (135 pages)</a> --- good luck. It takes a long time to edit
all these files by hand, and if you change your code you will end up
with docs which do not agree. Unless you constantly update all those
Rd files. (will you really?)</li>

<li><b>Option 2: <a href="http://roxygen.org/">Roxygen</a>.</b> You
like the idea of combining source code with documentation, because
then you can do "literate programming" and moreover, it is practically
easier to keep them in sync with each other. But ROxygen has a number
of drawbacks, especially for documenting function arguments. The huge
headers are usually far away from the actual code being documented,
and you have to repeat yourself when you name the function arguments
in the comment and the definition. (but to its credit, the call graphs
that Roxygen makes are pretty cool).</li>

<li><b>Option 3: inlinedocs.</b> No syntax to learn, never have to
repeat yourself, just comment your code in natural places, and you get
Rd files that pass R CMD check so you can publish that package and
move on with your life. And when you make changes to that function,
the docs are right next to the code so odds are you will update the
docs if they need to be.</li>

</ul>

<img src="inlinedocs-demo.png" />

<p>Then put your code in pkgdir/R/code.file.R, write a
pkgdir/DESCRIPTION, start R, and do:</p>

<pre>
install.packages("inlinedocs",repos="http://r-forge.r-project.org")
library(inlinedocs)
package.skeleton.dx("/path/to/your/pkgdir")
</pre>

<p>Then you are done! You will get Rd files in pkgdir/man that are
guaranteed to pass R CMD check! Some notes/tips:</p>

<ul>

<li>Use an editor where you can comment-fill easily, like <a
href="http://www.gnu.org/software/emacs">Emacs</a> with <a
href="http://ess.r-project.org">ESS</a>. Then you just type "###" and
go on as long as you want with your comment. When you're done and you
have a really long comment, do a M-q to automatically break lines and
add ### prefixes.</li>

<li>For additional flexibility, inlinedocs now includes additional
triggers based on the string "##<<". This means that most of the
standard documentation sections can be filled in from wherever is
relevant in the source code, as follows:

<ul>

<li> ##<< at the end of a line can be used to add function
argument descriptions on the same line as an argument name - any
immediately following ## comment lines will also be included.  </li>

<li> ##xxx<< as the first non-white space element on a line introduces
a section of consecutive ## comment lines which will be placed in the
"xxx" section of the documentation, where xxx is one of: alias,
details, keyword, references, author, note, seealso, value, title or
description. Multiple such chunks are concatenated appropriately - you
can think of this as something like the C++ "put" operator putting
chunks into that section.  For further details and examples of these
look for << in the source file <a
href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/inlinedocs/R/package.skeleton.dx.R?root=inlinedocs&view=markup">package.skeleton.dx.R</a>
</li>

<li> ##describe<< within such a documentation chunk allows a
"describe" block within that chunk to be constructed using same-line
##<< comments (as for the function arguments). All the following
source lines until another ##xxx<< line will be scanned for such
same-line comments. (##end<< may be used to end such a block and
return to the same documentation chunk.) This allows named lists to be
documented using the names actually in the code so that it is less
easy to forget to document when you add an element.</li>

</ul>
The package includes a <a
href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/inlinedocs/tests/silly/R/silly.example.R?root=inlinedocs&view=markup">silly example</a>, which (as the file name suggests) is a very silly function indeed but exercises most of these facilities.
</li>

<li>Some examples:

<ul>

<li>inlinedocs:
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/inlinedocs/R/package.skeleton.dx.R?root=inlinedocs&view=markup">package.skeleton.dx.R</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/inlinedocs/DESCRIPTION?root=inlinedocs&view=markup">DESCRIPTION</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/inlinedocs/man?root=inlinedocs">generated Rd files</a>
</li>

<li>sublogo:
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/R/sublogo.dendrogram.R?root=sublogo&view=markup">sublogo.dendrogram.R</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/DESCRIPTION?root=sublogo&view=markup">DESCRIPTION</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/man?root=sublogo">generated Rd files</a>
</li>

<li>latticedl:
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/latticedirectlabels/R/direct.labels.R?root=directlabels&view=markup">direct.labels.R</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/latticedirectlabels/R/positioning.functions.R?root=directlabels&view=markup">positioning.functions.R</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/latticedirectlabels/DESCRIPTION?root=directlabels&view=markup">DESCRIPTION</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/latticedirectlabels/man?root=directlabels">generated Rd files</a>
</li>

<li>nicholsonppp:
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/R/sim.R?root=nicholsonppp&view=markup">sim.R</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/R/plot.R?root=nicholsonppp&view=markup">plot.R</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/DESCRIPTION?root=nicholsonppp&view=markup">DESCRIPTION</a>
<a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/man?root=nicholsonppp">generated Rd files</a>
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

<li>For the title of the Rd file, function names are used. Dots and
underscores in the function name are translated into spaces in the
title. Or you can specify the title in a comment at the end of the
first line of the function definition, like this:

<pre>
package.skeleton.dx <- function # Package skeleton deluxe
</pre>

</li>

<li>The DESCRIPTION file is used by inlinedocs for the Maintainer
(this becomes the author section of the Rd files) and Package (this is
used as the name argument to package.skeleton). DESCRIPTION is also
used for constructing the pkgname-package Rd file, so you have to
include these fields: Version License Description Title Author. If
you're really lazy (like me), and you haven't written one yet,
inlinedocs will create an empty DESCRIPTION file for you.
</li>

<li>You still have to manually document your datasets (the files in
pkgdir/data), but these don't change often, so that's no problem
to do by hand. To get started just use the prompt() function.</li>

<li>To add examples for FUN, add a file FUN.R to your
pkgdir/tests directory. This is more sustainable than putting
test code in R comments, since debugging/changing commented code is a
pain.</li>

<li>inlinedocs is not as powerful as Roxygen, so if you want to use
all the features of Rd, Roxygen may be a better choice. However
inlinedocs is far simpler to learn and offers less repetitive
syntax.</li>

</ul>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
