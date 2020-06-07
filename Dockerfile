ARG RENDER_GIT_COMMIT
FROM taylorfausak/monadoc:$RENDER_GIT_COMMIT
ENV PORT 4444
CMD monadoc --host '*' --port $PORT
