# image base
FROM archlinux
#docker add
COPY ./scripts/judge.sh /home/
# docker env
# docker run
RUN pacman -Sy;\
    pacman -S rustup gdb lldb --noconfirm;\
    rustup install 1.15.1;\
    rustup default 1.15.1;\
    chmod +x /home/judge.sh;
# docker workdir
WORKDIR /
# docker cmd
CMD ["sh","/home/judge.sh"]
