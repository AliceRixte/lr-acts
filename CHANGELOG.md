# Changelog for `lr-acts`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.0 - 2025-05-22

### Added

- Left and right actions
- Semigroup, monoid and group actions
- Cyclic and generated actions
- Torsors
- Semidirect products

## 0.0.1 - 2025-05-24

- Fix deriving mechanism for Torsor instances

## 0.1 - 2025-11-29

- Rename LSemidirect and RSemidirect constructors to LPair and RPair
- Instances for ActCyclic x ()
- Add LDefault and RDefault newtypes for ActCyclic

## 0.1.1 - 2025-12-01

- Fix benchmark

## 0.2 - Unreleased

- Rename :
  - unactCoerce -> unactSelf'
  - unactId -> unactTrivial
