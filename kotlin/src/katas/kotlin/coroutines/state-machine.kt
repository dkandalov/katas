package katas.kotlin.coroutines

enum class State {
    Initial,
    GeneratedInvitation,
    SentInvitation,
    DeliveredInvitation,
}

class InvitationSender {
    private var state: State = State.Initial

    fun generateInvitation(): Int {
        println("generating invitation")
        state = State.GeneratedInvitation
        return 42
    }

    fun sendInvitation(inviteId: Int) {
        println("sending invitation $inviteId")
        state = State.SentInvitation
    }

    fun deliveryConfirmed(inviteId: Int) {
        println("done with delivery $inviteId")
        state = State.DeliveredInvitation
    }
}

fun main() {
    val invitationSender = InvitationSender()
    val id = invitationSender.generateInvitation()
    invitationSender.sendInvitation(id)
    invitationSender.deliveryConfirmed(id)
}