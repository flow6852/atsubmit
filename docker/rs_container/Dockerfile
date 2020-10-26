# image base
FROM archlinux
#docker add
COPY ./scripts/judge.sh /home/
# docker env
ENV RUST_BACKTRACE full
# docker run
RUN pacman -Sy --noconfirm;\
    pacman -S gdb lldb llvm rustup --noconfirm;\
    rustup default 1.42.0;\
    chmod +x /home/judge.sh;
# docker workdir
WORKDIR /
# docker cmd
CMD ["sh","/home/judge.sh"]
