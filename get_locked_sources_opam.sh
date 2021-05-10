revdeps()
{
	echo `opam list --depends-on=$1 --short --columns name`
	return 0
}


rd1=`revdeps $1 | tr ' ' ',' `


for package in `revdeps $rd1`
do
	opam source $package
done 

sources=`ls -d */|grep -v _opam`

locks=""

opam switch create ./ ocaml-system.4.10.2

eval $(opam env) 

rootpath=`pwd`

for source in $sources 
do
	cd $source &&
	opam monorepo lock && locks=$locks" "$source 
	cd $rootpath
done

echo $locks