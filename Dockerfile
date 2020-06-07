ARG RENDER_GIT_COMMIT
FROM docker.pkg.github.com/tfausak/monadoc/monadoc:$RENDER_GIT_COMMIT
ENV PORT 4444
CMD monadoc --host '*' --port $PORT
