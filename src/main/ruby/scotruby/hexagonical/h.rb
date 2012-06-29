class Organization
  def to_param
    "42"
  end
  def saved?
    rand > 0.5
  end
end

class OrganizationCreator
  def create_for(user, org_name, callbacks)
    # real creation logic here
    organization = Organization.new
    if organization.saved?
      callbacks[:success].call(organization)
    else
      callbacks[:failure].call(organization)
    end
  end
end

class OrganizationsController
  def create
    params = { organization: 'Hello' }
    creator = OrganizationCreator.new
    creator.create_for(current_user, params[:organization],
                       success: ->(org) { redirect_to organizations_path(org) },
                       failure: ->(org) { render :error }
    )
  end
  private
  def redirect_to(path)
    puts "redirecting_to #{path}"
  end

  def render(renderable)
    puts "rendering #{renderable}"
  end
  def current_user
    Object.new
  end
  def organizations_path(org)
    "/organizations/#{org.to_param}"
  end
end

controller = OrganizationsController.new
controller.create