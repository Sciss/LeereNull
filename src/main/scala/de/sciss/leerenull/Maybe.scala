/*
 *  Maybe.scala
 *  (LeereNull)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.leerenull

trait MaybeNot { implicit def none[ A ] : Maybe[ A ] = Maybe( None )}
object Maybe extends MaybeNot {
   implicit def some[ A ]( implicit a: A ) : Maybe[ A ] = Maybe( Some( a ))
   implicit def unwind[ A ]( mb: Maybe[ A ]) : Option[ A ] = mb.option
}
final case class Maybe[ A ]( option: Option[ A ])
