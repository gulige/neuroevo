#!/bin/sh

echo "Please enter VER"
read VER

cd ..

mkdir -p _tars_build/tarballs

#rebar3 as prod release -n neuroevo
rebar3 as prod tar -n neuroevo
cp _build/prod/rel/neuroevo/neuroevo-${VER}.tar.gz ./_tars_build/tarballs/

cd _tars_build
rm -rf NEUROEVO_V${VER}
mkdir NEUROEVO_V${VER}
cd NEUROEVO_V${VER}
cp ../tarballs/* .

mkdir neuroevo-${VER}
tar xf neuroevo-${VER}.tar.gz -C neuroevo-${VER}

rm -f *.tar.gz

cp ../../scripts_aux/run.sh ./.run.sh
echo "#!/bin/sh
echo \"Please enter neuroevo or all\"
read result
./.run.sh << EOF
${VER}
\${result}
EOF" > run.sh
chmod +x run.sh
chmod +x .run.sh

cp ../../scripts_aux/run_d.sh ./.run_d.sh
echo "#!/bin/sh
echo \"Please enter neuroevo or all\"
read result
./.run_d.sh << EOF
${VER}
\${result}
EOF" > run_d.sh
chmod +x run_d.sh
chmod +x .run_d.sh

cp ../../scripts_aux/stop.sh ./.stop.sh
echo "#!/bin/sh
echo \"Please enter neuroevo or all\"
read result
./.stop.sh << EOF
${VER}
\${result}
EOF" > stop.sh
chmod +x stop.sh
chmod +x .stop.sh

cd ..
tar -czf NEUROEVO_V${VER}.tar.gz NEUROEVO_V${VER}

echo "#!/bin/sh
echo \"Installing...\"
tar -xzvf NEUROEVO_V${VER}.tar.gz
echo \"Done!\"" > NEUROEVO_V${VER}_install.sh
chmod +x NEUROEVO_V${VER}_install.sh

echo "Done!"
