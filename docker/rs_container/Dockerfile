# image base
FROM archlinux
#docker add
COPY ./scripts/judge.sh /home
COPY ./Cargo.toml /Cargo.toml.tmp
COPY ./main.rs /main.rs.tmp
# docker env
ENV RUST_BACKTRACE full
ENV USER user
# docker run
# /home/atsubmit_rs
RUN pacman -Sy --noconfirm;\
    pacman -S gdb lldb llvm rustup glibc --noconfirm;\
    rustup default 1.42.0;\
    chmod +x /home/judge.sh;\
    cd /home;\
    cargo new atsubmit_rs;\
    cd atsubmit_rs;\
    cp /Cargo.toml.tmp Cargo.toml;\
    cargo build;
# docker workdir
WORKDIR /
# docker cmd
# CMD ["sh","/home/judge.sh"]
