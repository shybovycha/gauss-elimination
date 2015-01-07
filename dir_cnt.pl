use Cwd;$c=-2;opendir($d,cwd())||die"opendir(${cwd()}): $!";while($e=readdir($d)){$c++};closedir$d;print("${c}\n");
