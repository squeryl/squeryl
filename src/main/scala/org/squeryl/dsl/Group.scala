package org.squeryl.dsl

class Group[K](k: K) {
  def key = k
}

class Measures[M](m: M) {
  def measures = m
}

class GroupWithMeasures[K,M](k: K, m: M) {
  def key = k
  def measures = m
}
