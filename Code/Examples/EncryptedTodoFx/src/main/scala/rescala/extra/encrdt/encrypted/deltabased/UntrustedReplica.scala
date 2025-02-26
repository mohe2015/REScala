package rescala.extra.encrdt.encrypted.deltabased

import kofre.base.Lattice._

import kofre.time.Dots

import scala.collection.mutable

abstract class UntrustedReplica(initialDeltaGroups: Set[EncryptedDeltaGroup] = Set.empty) extends Replica {
  protected var dottedVersionVector: Dots                                  = Dots.empty
  protected var encryptedDeltaGroupStore: mutable.Set[EncryptedDeltaGroup] = mutable.Set.from(initialDeltaGroups)

  override def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    prune(encryptedDeltaGroup)

    dottedVersionVector = dottedVersionVector.merge(encryptedDeltaGroup.dottedVersionVector)
    encryptedDeltaGroupStore.add(encryptedDeltaGroup)
    ()
  }

  protected def prune(receivedEncryptedDeltaGroup: EncryptedDeltaGroup): Unit
}
