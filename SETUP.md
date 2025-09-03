# 개발 환경 설정

- OS: arch linux 2025-09-03
- 편집기: helix + SBT
- scala 2.10.7

1. coursier 설치
```
yay -S coursier
```

2. scala, scalac, sbt 설치

```
coursier setup
```

3. JDK 설치

- scala 2.10.7은 JDK 8에서만 정상 작동하는것으로 보임
- metals 등 다른 스칼라 기반 툴은 최신 JDK 필요

```
sudo pacman -S jdk8-openjdk jdk-openjdk
```

4. sbt 쉘 실행

- `TERM` 변수가 `xterm-color`가 아닐 경우 (내 경우에는 `xterm-ghostty`) 오류 발생 가능

```
export TERM=xterm-color
```

- JDK 8을 이용해서 sbt 쉘 실행

```
sbt -java-home /usr/lib/jvm/java-8-openjdk
```

5. metals 설치

- 스칼라의 LSP 구현체
- sbt 0.13.x 버전은 BSP를 지원하지 않아 metals LSP의 기능이 한정됨

```
yay -S metals
```
