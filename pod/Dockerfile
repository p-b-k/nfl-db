FROM  ubuntu
RUN   apt-get update
RUN   apt-get install -y sbcl curl librsvg2-bin
RUN   curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN   sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" \
                                 --eval "(exit)"
COPY  sbclrc /root/.sbclrc
COPY  stage/lisp/*.lisp .
COPY  stage/data ./data
COPY  stage/make-pngs make-pngs

RUN   ./make-pngs
RUN   /usr/bin/sbcl --load nfl-load.lisp --eval "(exit)"

CMD   /usr/bin/sbcl --load nfl-load.lisp --eval "(nfl-tool:run-league)" --eval "(exit)"
# CMD   /usr/bin/sbcl

